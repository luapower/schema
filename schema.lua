
--RDBMS schema definition language & operations
--Written by Cosmin Apreutesei. Public Domain.

if not ... then require'schema_test'; return end

local glue = require'glue'
require'$'

--definition parsing ---------------------------------------------------------

--NOTE: flag names clash with unquoted field names!
--NOTE: env names clash with all unquoted names!

local function isschema(t) return istab(t) and t.is_schema end

local schema = {is_schema = true, package = {}, isschema = isschema}

local function resolve_type(self, fld, t, i, n, fld_ct, allow_types, allow_flags)
	for i = i, n do
		local k = t[i]
		local v = k
		if isstr(v) then --type name, flag name or next field
			v = allow_types and self.types[k] or allow_flags and self.flags[k]
			if not v then --next field
				return i
			end
		end
		if isfunc(v) then --type generator
			v = v(self, fld_ct, fld, t[i])
		end
		if v then
			resolve_type(self, fld, v, 1, #v, fld_ct, true, true) --recurse
			for k,v in pairs(v) do --copy named attrs
				if isstr(k) then
					fld[k] = v
				end
			end
		end
	end
	return n + 1
end

local function parse_cols(self, t, dt, loc1, fld_ct)
	local i = 1
	while i <= #t do --[out], field_name, type_name, flag_name|{attr->val}, ...
		local col, mode
		if not fld_ct.is_table then --this is a proc param, not a table field.
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
		local fld = {col = col, mode = mode, col_index = #dt + 1}
		add(dt, fld)
		i = resolve_type(self, fld, t, i, i , fld_ct, true, false)
		i = resolve_type(self, fld, t, i, #t, fld_ct, false, true)
	end
end

--add convenience ref fields for automatic lookup.
--TODO: move this to xrowset_sql?
local function add_ref_col(self, tbl, fld, fk)
	if #fk.cols == 1 then
		local fk_tbl = fk.table == tbl.name and tbl or self.tables[fk.table]
		local fk_col = fk.cols[1]
		for _, fld in ipairs(fk_tbl.fields) do
			if fld.col == fk_col then
				fld.ref_table = name
				fld.ref_col   = tbl.pk[1]
			end
		end
	end
end

local function parse_table(self, name, t)
	local tbl = {is_table = true, name = name, fields = {}}
	parse_cols(self, t, tbl.fields, name, tbl)
	--resolve fks that ref this table.
	local fks = self.table_refs and self.table_refs[name]
	if fks then
		for fk in pairs(fks) do
			fk.ref_cols = tbl.pk
			add_ref_col(self, tbl, fld, fk)
		end
		self.table_refs[name] = nil
	end
	--add API to table to add more cols after-definition.
	function tbl.add_cols(t)
		parse_cols(self, t, tbl.fields, name, tbl)
	end
	return tbl
end

local function parse_ix_cols(fld, ...) --'col1 [desc], ...'
	if not ... then
		return {fld.col}
	end
	local s = cat({...}, ',')
	local dt = {desc = {}}
	for s in s:gmatch'[^,]+' do
		s = s:trim()
		local name, desc = s:match'(.-)%s+desc$'
		if name then
			desc = true
		else
			name, desc = s, false
		end
		add(dt, name)
		add(dt.desc, desc and true or false)
	end
	return dt
end

local function check_cols(T, tbl, cols)
	for i,col in ipairs(cols) do
		local found = false
		for i,fld in ipairs(tbl.fields) do
			if fld.col == col then found = true; break end
		end
		assertf(found, 'unknown column in %s of `%s`: `%s`', T, tbl.name, col)
	end
	return cols
end

local function add_pk(tbl, cols)
	assertf(not tbl.pk, 'pk already applied for table `%s`', tbl.name)
	tbl.pk = check_cols('pk', tbl, cols)
end

local function add_ix(T, tbl, cols)
	local t = attr(tbl, T..'s')
	local k = _('%s_%s__%s', T, tbl.name, cat(cols, '_'))
	assertf(not t[k], 'duplicate %s `%s`', T, k)
	t[k] = check_cols(T, tbl, cols)
end

local function add_fk(self, tbl, cols, ref_tbl, ondelete, onupdate, fld)
	local fks = attr(tbl, 'fks')
	local k = _('fk_%s__%s', tbl.name, cat(cols, '_'))
	assertf(not fks[k], 'duplicate fk `%s`', k)
	ref_tbl = ref_tbl or assert(#cols == 1 and cols[1])
	local fk = {table = tbl.name, cols = check_cols('fk', tbl, cols),
		ref_table = ref_tbl, ondelete = ondelete, onupdate = onupdate}
	fks[k] = fk
	attr(tbl, 'deps')[ref_tbl] = true
	local ref_tbl_t = self.tables[ref_tbl]
	if ref_tbl_t then
		fk.ref_cols = assertf(ref_tbl_t.pk, 'ref table `%s` has no PK', ref_tbl)
		--TODO: move this to xrowset_sql.
		if fld then
			fld.ref_table = ref_tbl
			fld.ref_col   = ref_tbl_t.pk[1]
		end
	else --we'll resolve it when the table is defined later.
		attr(attr(self, 'table_refs'), ref_tbl)[fk] = true
	end
end

do
	local function add_global(t, k, v)
		assertf(not t.flags [k], 'global overshadows flag `%s`', k)
		assertf(not t.types [k], 'global overshadows type `%s`', k)
		assertf(not t.tables[k], 'global overshadows table `%s`', k)
		assertf(not t.procs [k], 'global overshadows proc `%s`', k)
		rawset(t, k, v)
	end
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
		self.flags = update({}, schema.flags)
		self.types = update({}, schema.types)
		self.procs = {}
		env.flags = self.flags
		env.types = self.types
		init(self, env, 'tables', parse_table)
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

schema.env = {_G = _G}

do
	local function fk_func(force_ondelete, force_onupdate)
		return function(arg1, ...)
			if isschema(arg1) then --used as flag: make a fk on current field.
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
			if isschema(arg1) then --used as flag: make an index on current field.
				local self, tbl, fld = arg1, ...
				add_ix(T, tbl, {fld.col})
				fld[T] = true
			else --called by user, return a flag generator.
				local cols = pack(arg1, ...)
				return function(self, tbl, fld)
					local cols = parse_ix_cols(fld, unpack(cols))
					add_ix(T, tbl, cols)
				end
			end
		end
	end
	schema.env.uk = ix_func'uk'
	schema.env.ix = ix_func'ix'
end

schema.flags = {}
schema.types = {}

function schema.env.pk(arg1, ...)
	if isschema(arg1) then --used as flag.
		local self, tbl, cur_fld = arg1, ...
		add_pk(tbl, imap(tbl.fields, 'col'))
		--apply `not_null` flag to all fields up to this.
		for _,fld in ipairs(tbl.fields) do
			fld.not_null = true
			if fld == cur_fld then break end
		end
	else --called by user, return a flag generator.
		local cols = pack(arg1, ...)
		return function(self, tbl, fld)
			local cols = parse_ix_cols(fld, unpack(cols))
			add_pk(tbl, cols)
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

local function trigger_pos(tgs, when, op)
	local i = 1
	for _,tg in pairs(tgs) do
		if tg.when == when and tg.op == op then
			i = i + 1
		end
	end
	return i
end
function schema:add_trigger(name, when, op, tbl_name, ...)
	name = _('%s_%s_%s%s', tbl_name, name, when:sub(1,1), op:sub(1,1))
	local tbl = assertf(self.tables[tbl_name], 'unknown table `%s`', tbl_name)
	local triggers = attr(tbl, 'triggers')
	assertf(not triggers[name], 'duplicate trigger `%s`', name)
	triggers[name] = update({name = name, when = when, op = op,
		table = tbl_name, pos = trigger_pos(triggers, when, op)}, ...)
end

function schema:add_proc(name, args, ...)
	local p = {name = name, args = {}}
	parse_cols(self, args, p.args, name, p)
	update(p, ...)
	self.procs[name] = p
end

function schema:add_cols(...)
	--TODO:
end

schema.env.null = function() end

function schema:check_refs()
	if not self.table_refs or not next(self.table_refs) then return end
	assertf(false, 'unresolved refs to tables: %s', cat(keys(self.table_refs, true), ', '))
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
			attr(dt, 'remove')[k] = v2
		end
	end
	for k,v1 in pairs(t1) do
		local v2 = t2[k]
		if v2 == nil then
			attr(dt, 'add')[k] = v1
		elseif diff_vals then
			local vdt = diff_vals(self, v1, v2, sc2)
			if vdt == true then
				attr(dt, 'remove')[k] = v2
				attr(dt, 'add'   )[k] = v1
			elseif vdt then
				attr(dt, 'update')[k] = vdt
			end
		end
	end
	return next(dt) and dt or nil
end

local function diff_arrays(a1, a2)
	a1 = a1 or empty
	a2 = a2 or empty
	if #a1 ~= #a2 then return end
	for i,s in ipairs(a1) do
		if a2[i] ~= s then return end
	end
	return true
end
local function diff_ix_cols(self, c1, c2)
	return diff_arrays(c1, c2) or diff_arrays(c1.desc, c2.desc)
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
	return diff_keys(self, f1, f2, sc2.relevant_field_attrs)
end

local function diff_ixs(self, ix1, ix2)
	return diff_ix_cols(self, ix1, ix2) or ix1.desc ~= ix2.desc
end

local function diff_fks(self, fk1, fk2)
	return diff_keys(self, fk1, fk2, {
		table=1,
		ref_table=1,
		onupdate=1,
		ondelete=1,
		cols=function(self, c1, c2) return diff_ix_cols(self, c1, c2) end,
		ref_cols=function(self, c1, c2) return diff_ix_cols(self, c1, c2) end,
	})
end

local function diff_checks(self, c1, c2)
	local BODY = self.engine..'_body'
	return c1[BODY] ~= c2[BODY] or c1.body ~= c2.body
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
	local pk   = diff_maps(self, {pk=t1.pk} , {pk=t2.pk} , diff_ix_cols  , nil, sc2)
	d.uks      = diff_maps(self, t1.uks     , t2.uks     , diff_ix_cols  , nil, sc2)
	d.ixs      = diff_maps(self, t1.ixs     , t2.ixs     , diff_ixs      , nil, sc2)
	d.fks      = diff_maps(self, t1.fks     , t2.fks     , diff_fks      , nil, sc2, sc2.supports_fks      or false)
	d.checks   = diff_maps(self, t1.checks  , t2.checks  , diff_checks   , nil, sc2, sc2.supports_checks   or false)
	d.triggers = diff_maps(self, t1.triggers, t2.triggers, diff_triggers , nil, sc2, sc2.supports_triggers or false)
	d.add_pk    = pk and pk.add and pk.add.pk
	d.remove_pk = pk and pk.remove and pk.remove.pk
	if not next(d) then return nil end
	d.old = t2
	d.new = t1
	return d
end

local diff = {is_diff = true}

function schema:diff_from_old(sc2) --sync sc2 to self.
	local sc1 = self
	local sc2 = assertf(isschema(sc2) and sc2, 'schema expected, got `%s`', type(sc2))
	sc1:check_refs()
	sc2:check_refs()
	local self = {engine = sc2.engine, __index = diff}
	self.tables = diff_maps(self, sc1.tables, sc2.tables, diff_tables, nil, sc2)
	self.procs  = diff_maps(self, sc1.procs , sc2.procs , diff_procs , nil, sc2, sc2.supports_procs or false)
	return setmetatable(self, self)
end

function schema:diff_to_new(sc2) --sync self to sc2.
	return sc2:diff_from_old(self)
end

local function dots(s, n) return #s > n and s:sub(1, n-2)..'..' or s end
local tobytes = function(x) return x and glue.tobytes(x) or '' end
local function P(...) print(_(...)) end
function diff:pp(opt)
	local BODY = self.engine..'_body'
	print()
	local function pp_fld(fld, prefix)
		P(' %1s %3s %-2s%-16s %-8s %4s%1s %6s %6s %-18s %s',
			fld.auto_increment and 'A' or '',
			prefix or '',
			fld.not_null and '*' or '',
			dots(fld.col, 16), fld.type or '',
			fld.type == 'number' and not fld.digits and '['..fld.size..']'
			or fld.type == 'bool' and ''
			or (fld.digits or '')..(fld.decimals and ','..fld.decimals or ''),
			fld.type == 'number' and not fld.unsigned and '-' or '',
			tobytes(fld.size) or '', tobytes(fld.maxlen) or '',
			fld[self.engine..'_collation'] or '',
			fld[self.engine..'_default'] or ''
		)
	end
	local function format_fk(fk)
		return _('(%s) -> %s (%s)%s%s', cat(fk.cols, ','), fk.ref_table,
				cat(fk.ref_cols, ','),
				catargs(' D:', fk.ondelete) or '',
				catargs(' U:', fk.onupdate) or ''
			)
	end
	local function ix_cols(ix)
		local dt = {}
		for i,s in ipairs(ix) do
			dt[i] = s .. (ix.desc and ix.desc[i] and ':desc' or '')
		end
		return cat(dt, ',')
	end
	local function pp_tg(tg, prefix)
		P('   %1sTG %d %s %s `%s`', prefix or '', tg.pos, tg.when, tg.op, tg.name)
		if prefix ~= '-' then
			print(outdent(tg[BODY], '         '))
		end
	end
	if self.tables and self.tables.add then
		for tbl_name, tbl in sortedpairs(self.tables.add) do
			P(' %-24s %-8s %2s,%-1s%1s %6s %6s %-18s %s', '+ TABLE '..tbl_name,
				'type', 'D', 'd', '-', 'size', 'maxlen', 'collation', 'default')
			print(('-'):rep(80))
			local pk = tbl.pk and index(tbl.pk)
			for i,fld in ipairs(tbl.fields) do
				local pki = pk and pk[fld.col]
				local desc = pki and tbl.pk.desc and tbl.pk.desc[pki]
				pp_fld(fld, pki and _('%sK%d', desc and 'p' or 'P', pki))
			end
			print('    -------')
			if tbl.uks then
				for uk_name, uk in sortedpairs(tbl.uks) do
					P('    UK   %s', ix_cols(uk))
				end
			end
			if tbl.ixs then
				for ix_name, ix in sortedpairs(tbl.ixs) do
					P('    IX   %s', ix_cols(ix))
				end
			end
			if tbl.fks then
				for fk_name, fk in sortedpairs(tbl.fks) do
					P('    FK   %s', format_fk(fk))
				end
			end
			if tbl.checks then
				for ck_name, ck in sortedpairs(tbl.checks) do
					P('    CK   %s', ck[BODY] or ck.body)
				end
			end
			local tgs = tbl.triggers
			if tgs then
				local function cmp_tg(tg1, tg2)
					local a = tgs[tg1]
					local b = tgs[tg2]
					if a.op ~= b.op then return a.op < b.op end
					if a.when ~= b.when then return a.when < b.when end
					return a.pos < b.pos
				end
				for tg_name, tg in sortedpairs(tgs, cmp_tg) do
					pp_tg(tg)
				end
			end
			print()
		end
	end
	if self.tables and self.tables.update then
		local hide_attrs = opt and opt.hide_attrs
		for tbl_name, d in sortedpairs(self.tables.update) do
			P(' ~ TABLE %s', tbl_name)
			print(('-'):rep(80))
			if d.fields and d.fields.add then
				for col, fld in sortedpairs(d.fields.add) do
					pp_fld(fld, '+')
				end
			end
			if d.fields and d.fields.remove then
				for col, fld in sortedpairs(d.fields.remove) do
					pp_fld(fld, '-')
				end
			end
			if d.fields and d.fields.update then
				for col, d in sortedpairs(d.fields.update) do
					pp_fld(d.old, '<')
					pp_fld(d.new, '>')
				end
			end
			if d.remove_pk then
					P('   -PK   %s', ix_cols(d.remove_pk))
			end
			if d.add_pk then
					P('   +PK   %s', ix_cols(d.add_pk))
			end
			if d.uks and d.uks.remove then
				for uk_name, uk in sortedpairs(d.uks.remove) do
					P('   -UK   %s', ix_cols(uk))
				end
			end
			if d.uks and d.uks.add then
				for uk_name, uk in sortedpairs(d.uks.add) do
					P('   +UK   %s', ix_cols(uk))
				end
			end
			if d.ixs and d.ixs.remove then
				for ix_name, ix in sortedpairs(d.ixs.remove) do
					P('   -IX   %s', ix_cols(ix))
				end
			end
			if d.ixs and d.ixs.add then
				for ix_name, ix in sortedpairs(d.ixs.add) do
					P('   +IX   %s', ix_cols(ix))
				end
			end
			if d.checks and d.checks.remove then
				for ck_name, ck in sortedpairs(d.checks.remove) do
					P('   -CK   %s', ck[BODY] or ck.body)
				end
			end
			if d.checks and d.checks.add then
				for ck_name, ck in sortedpairs(d.checks.add) do
					P('   +CK   %s', ck[BODY] or ck.body)
				end
			end
			if d.fks and d.fks.remove then
				for fk_name, fk in sortedpairs(d.fks.remove) do
					P('   -FK   %s', format_fk(fk))
				end
			end
			if d.fks and d.fks.add then
				for fk_name, fk in sortedpairs(d.fks.add) do
					P('   +FK   %s', format_fk(fk))
				end
			end
			if d.triggers and d.triggers.remove then
				for tg_name, tg in sortedpairs(d.triggers.remove) do
					pp_tg(tg, '-')
				end
			end
			if d.triggers and d.triggers.add then
				for trg_name, trg in sortedpairs(d.triggers.add) do
					pp_tg(tg, '+')
				end
			end
			print()
		end
	end
	if self.tables and self.tables.remove then
		for tbl_name in sortedpairs(self.tables.remove) do
			P('  - TABLE %s', tbl_name)
		end
		print()
	end
	if self.procs and self.procs.remove then
		for proc_name in sortedpairs(self.procs.remove) do
			P(' - PROC %s', proc_name)
		end
		print()
	end
	if self.procs and self.procs.add then
		for proc_name, proc in sortedpairs(self.procs.add) do
			local args = {}; for i,arg in ipairs(proc.args) do
				args[i] = _('%s %s %s', arg.mode or 'in', arg.col,
					''
					--fld.auto_increment and 'A' or '',
					--prefix or '',
					--fld.not_null and '*' or '',
					--dots(fld.col, 16), fld.type or '',
					--fld.type == 'number' and not fld.digits and '['..fld.size..']'
					--or fld.type == 'bool' and ''
					--or (fld.digits or '')..(fld.decimals and ','..fld.decimals or ''),
					--fld.type == 'number' and not fld.unsigned and '-' or '',
					--tobytes(fld.size) or '', tobytes(fld.maxlen) or '',
					--fld[self.engine..'_collation'] or '',
				)
			end
			P('  + PROC %s(\n\t%s\n)\n%s', proc_name, cat(args, ',\n\t'), proc[BODY])
		end
		print()
	end
end

return schema
