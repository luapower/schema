
--RDBMS schema definition language & operations
--Written by Cosmin Apreutesei. Public Domain.

if not ... then require'schema_test'; return end

local glue = require'glue'
require'$'

--definition parsing ---------------------------------------------------------

local schema = {is_schema = true, package = {}}

local function istype  (t) return istab(t) and t.is_type end
local function isschema(t) return istab(t) and t.is_schema end

local function parse_flag(schema, name, t)
	return t
end

local function parse_type(self, name, t)
	local super = t[1]
	if isstr(super) then
		super = assertf(self.types[super], 'unknown super type `%s` for type `%s`', super, name)
	elseif super then
		assertf(istype(super), 'super type for type `%s` is not a type', name)
	end
	local flags = extend({}, super and super.flags)
	append(flags, unpack(t, 2))
	local typ = merge({is_type = true, flags = flags}, super)
	for k,v in pairs(t) do
		if not (isnum(k) and k >= 1) then
			typ[k] = v
		end
	end
	self.env.lower_type(self, typ) --not yet used, but cuould be useful.
	return typ
end

local function resolve_flag(self, flag, loc1, loc2, fld_ct, fld)
	local v = flag --assume custom flag: table of attributes.
	if isstr(flag) then --flag name: look it up.
		v = self.flags[flag]
		if not v then
			return nil, 'unknown flag `%s` at `%s.%s`', flag, loc1, loc2
		end
	end
	if isfunc(v) then --flag is actually flag generator.
		v = v(self, fld_ct, fld)
	end
	return v or empty
end

local function add_cols(self, t, dt, loc1, fld_ct)
	local i = 1
	while i <= #t do --field_name, field_type, field_flag|field_attrs, ...
		local col, mode
		if not fld_ct.is_table then --proc param
			mode = t[i]
			if mode == 'out' then
				i = i + 1
			else
				mode = nil --'in' (default)
			end
		end
		col = t[i]
		assertf(isstr(col), 'column name expected for `%s`, got %s', loc1, type(col))
		i = i + 1
		local typ = t[i]
		if isstr(typ) then
			typ = assertf(self.types[typ], 'unknown type `%s` for `%s.%s`', typ, loc1, col)
		else
			assertf(istype(typ), 'type for `%s.%s` is not a type', loc1, col)
		end
		i = i + 1
		local fld = {col = col, mode = mode, col_index = #dt + 1}
		add(dt, fld)
		--apply type attrs.
		update(fld, typ)
		--apply flags from type def.
		if typ.flags then
			for i, flag in ipairs(typ.flags) do
				local attrs = assertf(resolve_flag(self, flag, loc1, col, fld_ct, fld))
				update(fld, attrs)
			end
		end
		--apply flags from field def.
		while i <= #t do
			local flag = t[i]
			local attrs = resolve_flag(self, flag, loc1, col, fld_ct, fld)
			if not attrs then --next field or error
				break
			end
			update(fld, attrs)
			i = i + 1
		end
		fld.is_type = nil
		fld.flags = nil
	end
end
local function parse_table(self, name, t)
	local tbl = {is_table = true, name = name, fields = {}}
	add_cols(self, t, tbl.fields, name, tbl)
	function tbl.add_cols(t)
		add_cols(self, t, tbl.fields, name, tbl)
	end
	--resolve fks that ref this table.
	local fks = self.table_refs and self.table_refs[name]
	if fks then
		for fk in pairs(fks) do
			fk.ref_cols = tbl.pk
			--add convenience ref fields for automatic lookup.
			--TODO: move this to xrowset_sql.
			if #fk.cols == 1 then
				local fk_tbl = fk.table == name and tbl or self.tables[fk.table]
				local fk_col = fk.cols[1]
				for _, fld in ipairs(fk_tbl.fields) do
					if fld.col == fk_col then
						fld.ref_table = name
						fld.ref_col   = tbl.pk[1]
					end
				end
			end
		end
		self.table_refs[name] = nil
	end
	return tbl
end

local function parse_proc(self, name, t)
	return t
end

local function add_global(t, k, v)
	assertf(not t.flags [k], 'global overshadows flag `%s`', k)
	assertf(not t.types [k], 'global overshadows type `%s`', k)
	assertf(not t.tables[k], 'global overshadows table `%s`', k)
	assertf(not t.procs [k], 'global overshadows proc `%s`', k)
	rawset(t, k, v)
end

local function cols(s, newsep)
	return s:gsub('%s+', newsep or ',')
end
local function dename(s)
	return s:gsub('`', '')
end
local function indexname(type, tbl, col, suffix)
	return fmt('%s_%s__%s%s', type, dename(tbl), dename(cols(col, '_')))
end

local function add_fk(self, tbl, cols, ref_tbl, ondelete, onupdate, fld)
	local fks = attr(tbl, 'fks')
	local k = _('fk_%s__%s', tbl.name, cat(cols, '_'))
	assertf(not fks[k], 'duplicate fk `%s`', k)
	ref_tbl = ref_tbl or assert(#cols == 1 and cols[1])
	local fk = {table = tbl.name, cols = cols,
		ref_table = ref_tbl, ondelete = ondelete, onupdate = onupdate}
	fks[k] = fk
	attr(tbl, 'deps')[ref_tbl] = true
	local ref_tbl_t = self.tables[ref_tbl]
	if ref_tbl_t then
		fk.ref_cols = ref_tbl_t.pk
		--TODO: move this to xrowset_sql.
		if fld then
			fld.ref_table = ref_tbl
			fld.ref_col   = ref_tbl_t.pk[1]
		end
	else --we'll resolve it when the table is defined later.
		attr(attr(self, 'table_refs'), ref_tbl)[fk] = true
	end
end

local function add_ix(T, tbl, cols)
	local t = attr(tbl, T..'s')
	local k = _('%s_%s__%s', T, tbl.name, cat(cols, '_'))
	assertf(not t[k], 'duplicate %s `%s`', T, k)
	t[k] = cols
end

do
	local T = function() end
	local function getter(t, k) return t[T][k] end
	local function init(self, env, k, parse)
		local k1 = k:gsub('s$', '')
		local t = update({}, schema[k])
		self[k] = t
		env[k] = setmetatable({}, {
			__index = t,
			__newindex = function(_, k, v)
				assertf(not t[k], 'duplicate %s `%s`', k1, k)
				t[k] = parse(self, k, v)
			end,
		})
	end
	function schema.new(opt)
		assert(opt ~= schema, 'use dot notation')
		local self = update(opt or {}, schema)
		local env = update({self = self}, schema.env)
		init(self, env, 'flags' , parse_flag)
		init(self, env, 'types' , parse_type)
		init(self, env, 'tables', parse_table)
		init(self, env, 'procs' , parse_proc)
		local function resolve_symbol(t, k)
			return k --symbols resolve to their name as string.
		end
		setmetatable(env, {__index = resolve_symbol, __newindex = add_global})
		self.env = env
		self.loaded = {}

		function env.import      (...) self:import      (...) end
		function env.add_fk      (...) self:add_fk      (...) end
		function env.trigger     (...) self:add_trigger (...) end
		function env.proc        (...) self:add_proc    (...) end

		return self
	end
end

do
	local function import(self, k, sc)
		local k1 = k:gsub('s$', '')
		for k,v in pairs(sc[k]) do
			assertf(not self[k], 'duplicate %s `%s`', k1, k)
			self[k] = v
		end
	end
	function schema:import(src)
		if isstr(src) then --module
			src = schema.package[src] or require(src)
		end
		if isfunc(src) then --def
			if not self.loaded[src] then
				setfenv(src, self.env)
				src()
				self.loaded[src] = true
			end
		elseif isschema(src) then --schema
			if not self.loaded[src] then
				import(self, 'flags' , sc)
				import(self, 'types' , sc)
				import(self, 'tables', sc)
				import(self, 'procs' , sc)
				self.loaded[src] = true
			end
		elseif istab(src) then --plain table: use as environsment.
			update(self.env, src)
		else
			assert(false)
		end
		return self
	end
end

schema.env = {lower_type = noop, _G = _G}
schema.flags = {}

do
	local function fk_func(force_ondelete, force_onupdate)
		return function(arg1, ...)
			if isschema(arg1) then --used as flag.
				local self, tbl, fld = arg1, ...
				add_fk(self, tbl, {fld.col}, nil,
					force_ondelete,
					force_onupdate,
					fld)
			else --called by user, return a flag generator.
				local ref_tbl, ondelete, onupdate = arg1, ...
				return function(self, tbl, fld)
					add_fk(self, tbl, {fld.col}, ref_tbl,
						force_ondelete or ondelete,
						force_onupdate or onupdate,
						fld)
				end
			end
		end
	end
	schema.env.fk       = fk_func()
	schema.env.child_fk = fk_func'cascade'
	schema.env.weak_fk  = fk_func'set null'
end

function schema:add_fk(tbl, cols, ...)
	local tbl = assertf(self.tables[tbl], 'unknown table `%s`', tbl)
	add_fk(self, tbl, names(cols), ...)
end

do
	local function ix_func(T)
		return function(arg1, ...)
			if isschema(arg1) then --used as flag.
				local sch, tbl, fld = arg1, ...
				add_ix(T, tbl, {fld.col})
				fld[T] = true
			else --called by user, return a flag generator.
				local cols = names(cat({arg1, ...}, ' '))
				return function(sch, tbl, fld)
					if #cols == 0 then cols = {fld.col} end
					add_ix(T, tbl, cols)
				end
			end
		end
	end
	schema.env.uk = ix_func'uk'
	schema.env.ix = ix_func'ix'
end

function schema.flags.pk(self, tbl, fld)
	assertf(not tbl.pk, 'pk already applied for table `%s`', tbl.name)
	tbl.pk = imap(tbl.fields, 'col')
	if self.flags.not_null then --apply `not_null` flag to all fields up to this.
		for _,fld in ipairs(tbl.fields) do
			merge(fld, resolve_flag(self, 'not_null', tbl.name, fld.col, tbl, fld))
		end
	end
end

function schema.env.default(v)
	return {default = v}
end

function schema.env.check(body)
	return function(self, tbl, fld)
		local name = _('ck_%s__%s', tbl.name, fld.col)
		local ck = {}
		if istab(body) then
			update(ck, body) --mysql'...'
		else
			ck.body = body
		end
		attr(tbl, 'checks')[name] = ck
	end
end

function schema:add_trigger(name, when, op, tbl_name, ...)
	local tbl = assertf(self.tables[tbl_name], 'unknown table `%s`', tbl_name)
	attr(tbl, 'triggers')[name] = update({name = name, when = when, op = op, table = tbl_name}, ...)
end

function schema:add_proc(name, args, ...)
	local p = {name = name, args = {}}
	add_cols(self, args, p.args, name, p)
	update(p, ...)
	self.procs[name] = p
end

function schema:add_cols(...)
	--TODO:
end

schema.env.null = function() end

--dependency order -----------------------------------------------------------

local function dependency_order(items, item_deps)
	local function dep_maps()
		local t = {} --{item->{dep_item->true}}
		local function add_item(item)
			if t[item] then return true end --already added
			local deps = item_deps(item)
			local dt = {}
			t[item] = dt
			for dep_item in pairs(deps) do
				if add_item(dep_item) then
					dt[dep_item] = true
				end
			end
			return true --added
		end
		for item in pairs(items) do
			add_item(item)
		end
		return t
	end
	--add items with zero deps first, remove them from the dep maps
	--of all other items and from the original table of items,
	--and repeat, until there are no more items.
	local t = dep_maps()
	local circular_deps
	local dt = {}
	while next(t) do
		local guard = true
		for item, deps in sortedpairs(t) do --stabilize the list
			if not next(deps) then
				guard = false
				add(dt, item) --build it
				t[item] = nil --remove it from the final table
				--remove it from all dep lists
				for _, deps in pairs(t) do
					deps[item] = nil
				end
			end
		end
		if guard then
			circular_deps = t --circular dependencies found
			break
		end
	end
	return dt, circular_deps
end

function schema:table_create_order()
	return dependency_order(self.tables, function(tbl)
		return self.tables[tbl].deps or empty
	end)
end

function schema:check_refs()
	if not self.table_refs or not next(self.table_refs) then return end
	assertf(false, 'unresolved refs to tables: %s', keys(self.table_refs, true))
end

--schema diff'ing ------------------------------------------------------------

local function map_fields(flds)
	local t = {}
	for i,fld in ipairs(flds) do
		t[fld.col] = fld
	end
	return t
end

local function diff_maps(self, t1, t2, diff_vals, map, sc2, supported) --sync t2 to t1.
	if supported == false then return nil end
	t1 = t1 and (map and map(t1) or t1) or empty
	t2 = t2 and (map and map(t2) or t2) or empty
	local dt = {}
	for k,v2 in pairs(t2) do
		local v1 = t1[k]
		if v1 == nil then
			attr(dt, 'remove')[k] = true
		end
	end
	for k,v1 in pairs(t1) do
		local v2 = t2[k]
		if v2 == nil then
			attr(dt, 'add')[k] = v1
		elseif diff_vals then
			local vdt = diff_vals(self, v1, v2, sc2)
			if vdt == true then
				attr(dt, 'remove')[k] = true
				attr(dt, 'add'   )[k] = v1
			elseif vdt then
				attr(dt, 'update')[k] = vdt
			end
		end
	end
	return next(dt) and dt or nil
end

local function diff_col_lists(self, c1, c2)
	return cat(c1 or empty, '\0') ~= cat(c2 or empty, '\0')
end

local function not_eq(_, a, b) return a ~= b end
local function diff_keys(self, t1, t2, keys)
	local dt = {}
	for k, diff in pairs(keys) do
		if not isfunc(diff) then diff = not_eq end
		if diff(self, t1[k], t2[k]) then
			dt[k] = true
		end
	end
	return next(dt) and {old = t2, new = t1, changed = dt}
end

local function diff_fields(self, f1, f2, sc2)
	return sc2.compare_fields(f1, f2)
end

local function diff_ixs(self, ix1, ix2)
	return diff_col_lists(self, ix1, ix2) or ix1.desc ~= ix2.desc
end

local function diff_fks(self, fk1, fk2)
	return diff_keys(self, fk1, fk2, {
		table=1,
		ref_table=1,
		onupdate=1,
		ondelete=1,
		cols=function(self, c1, c2) return diff_col_lists(self, c1, c2) end,
		ref_cols=function(self, c1, c2) return diff_col_lists(self, c1, c2) end,
	})
end

local function diff_checks(self, c1, c2)
	return diff_keys(self, c1, c2, {[self.engine..'_body']=1})
end

local function diff_triggers(self, t1, t2)
	return diff_keys(self, t1, t2, {
		pos=1,
		when=1,
		op=1,
		[self.engine..'_body']=1,
	})
end

local function diff_procs(self, p1, p2, sc2)
	return diff_keys(self, p1, p2, {
		[self.engine..'_body']=1,
		args=function(self, a1, a2)
			return diff_maps(self, a1, a2, diff_fields, map_fields, sc2) and true
		end,
	})
end

local function diff_tables(self, t1, t2, sc2)
	local d = {}
	d.fields   = diff_maps(self, t1.fields  , t2.fields  , diff_fields   , map_fields, sc2)
	local pk   = diff_maps(self, {pk=t1.pk} , {pk=t2.pk} , diff_col_lists, nil, sc2)
	d.uks      = diff_maps(self, t1.uks     , t2.uks     , diff_col_lists, nil, sc2)
	d.ixs      = diff_maps(self, t1.ixs     , t2.ixs     , diff_ixs      , nil, sc2)
	d.fks      = diff_maps(self, t1.fks     , t2.fks     , diff_fks      , nil, sc2, sc2.supports_all or sc2.supports_fks      or false)
	d.checks   = diff_maps(self, t1.checks  , t2.checks  , diff_checks   , nil, sc2, sc2.supports_all or sc2.supports_checks   or false)
	d.triggers = diff_maps(self, t1.triggers, t2.triggers, diff_triggers , nil, sc2, sc2.supports_all or sc2.supports_triggers or false)
	d.add_pk    = pk and pk.add and pk.add.pk
	d.remove_pk = pk and pk.remove and pk.remove.pk
	if not next(d) then return nil end
	d.old = t2
	d.new = t1
	return d
end

local diff = {is_diff = true}

function schema:diff(sc2, opt) --sync sc2 to sc1.
	sc1 = self
	sc2 = sc2 or schema.new{engine = opt.engine, supports_all = true}
	sc1:check_refs()
	sc2:check_refs()
	local self = {engine = sc2.engine, __index = diff}
	self.tables = diff_maps(self, sc1.tables, sc2.tables, diff_tables, nil, sc2)
	self.procs  = diff_maps(self, sc1.procs , sc2.procs , diff_procs , nil, sc2, sc2.supports_all or sc2.supports_procs or false)
	return setmetatable(self, self)
end

local function dots(s, n) return #s > n and s:sub(1, n-2)..'..' or s end
local function bigsize(n)
	if not n then return end
	local e = ln(n + 1) / ln(2)
	if floor(e) == e then return '2^'..floor(e) end
	return n
end
function diff:pp(opt)
	if self.tables and self.tables.add then
		for tbl_name, tbl in sortedpairs(self.tables.add) do
			print(_('%-18s %-8s %2s,%-1s%1s %8s %8s', 'TABLE '..tbl_name,
				'type', 'D', 'd', '-', 'size', 'maxlen'))
			print(('-'):rep(78))
			for i,fld in ipairs(tbl.fields) do
				print(_('  %-16s %-8s %4s%1s %8s %8s',
					dots(fld.col, 16), fld.type or '',
					fld.type == 'number' and not fld.digits and '['..fld.size..']'
					or fld.type == 'bool' and ''
					or (fld.digits or '')..(fld.decimals and ','..fld.decimals or ''),
					fld.type == 'number' and not fld.unsigned and '-' or '',
					bigsize(fld.size) or '', bigsize(fld.maxlen) or ''))
			end
			if tbl.uks then
				for uk_name, uk in sortedpairs(tbl.uks) do
					print(_('  * UK %s', cat(uk, ' ')))
				end
			end
			if tbl.ixs then
				for ix_name, ix in sortedpairs(tbl.ixs) do
					print(_('  * IX %s%s', cat(ix, ' '), ix.desc and ' desc' or ''))
				end
			end
			if tbl.fks then
				for fk_name, fk in sortedpairs(tbl.fks) do
					print(_('  * FK %s -> %s (%s)',
						cat(fk.cols, ' '), fk.ref_table, cat(fk.ref_cols, ' ')))
				end
			end
			if tbl.checks then
				for ck_name, ck in sortedpairs(tbl.checks) do
					print(_('  * CK %s', ck[self.engine..'_body'] or ck.body))
				end
			end
			if tbl.triggers then
				for tg_name, tg in sortedpairs(tbl.triggers) do
					print(_('  * TG %s', tg[self.engine..'_body'] or tg.body))
				end
			end
			print()
		end
	end
	if self.tables and self.tables.update then
		local hide_attrs = opt and opt.hide_attrs
		for tbl_name, d in sortedpairs(self.tables.update) do
			print('TABLE CHANGED', tbl_name)
			print(('-'):rep(78))
			if d.fields and d.fields.update then
				for col, d in sortedpairs(d.fields.update) do
					local dt = {}
					for k in sortedpairs(d.changed) do
						if not hide_attrs or not hide_attrs[k] then
							add(dt, _('    %-16s %s -> %s', k,
								pp.format(d.old[k]),
								pp.format(d.new[k])
							))
						end
					end
					if #dt > 0 then
						print(_('  %-16s', col))
						imap(dt, print)
					end
				end
			end
			print()
		end
	end
	if self.tables and self.tables.remove then
		for tbl_name in sortedpairs(self.tables.remove) do
			print('TABLE REMOVED', tbl_name)
		end
	end
	if self.procs and self.procs.add then

	end
end

return schema
