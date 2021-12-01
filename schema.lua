
--RDBMS schema definition language & operations
--Written by Cosmin Apreutesei. Public Domain.

if not ... then require'schema_test'; return end

local glue = require'glue'
require'$'

--definition parsing ---------------------------------------------------------

local schema = {is_schema = true}

local function istype  (t) return istab(t) and t.is_type end
local function isschema(t) return istab(t) and t.is_schema end

local function parse_flag(schema, name, t)
	return t
end

local function parse_type(schema, name, t)
	local super = t[1]
	if isstr(super) then
		super = assertf(schema.types[super], 'unknown super type `%s` for type `%s`', super, name)
	elseif super then
		assertf(istype(super), 'super type for type `%s` is not a type', name)
	end
	local flags = extend({}, super and super.flags)
	append(flags, unpack(t, 2))
	local typ = merge({is_type = true, ['is_'..name] = true, flags = flags}, super)
	for k,v in pairs(t) do
		if not (isnum(k) and k >= 1) then
			typ[k] = v
		end
	end
	return typ
end

local function resolve_flag(sch, tbl, fld, flag)
	local v = flag --assume custom flag: table of attributes.
	if isstr(flag) then --flag name: look it up.
		v = sch.flags[flag]
		if not v then
			return nil, 'unknown flag `%s` for `%s.%s`', flag, tbl.name, fld.name
		end
	end
	if isfunc(v) then --flag is actually flag generator.
		v = v(sch, tbl, fld)
	end
	return v or empty
end

local function add_cols(schema, tbl, t)
	local i = 1
	while i <= #t do --field_name, field_type, field_flag|field_attrs, ...
		local col = t[i]
		assertf(isstr(col), 'column name expected for `%s`, got %s', name, type(col))
		i = i + 1
		local typ = t[i]
		if isstr(typ) then
			typ = assertf(schema.types[typ], 'unknown type `%s` for `%s.%s`', typ, name, col)
		else
			assertf(istype(typ), 'type for `%s.%s` is not a type', name, col)
		end
		i = i + 1
		local fld = merge({name = col}, typ)
		add(tbl.fields, fld)
		while i <= #t do
			local flag = t[i]
			local attrs = resolve_flag(schema, tbl, fld, flag)
			if not attrs then --next field or error
				break
			end
			update(fld, attrs)
			i = i + 1
		end
		--apply flag generators from types after the more specific ones in the field.
		if typ.flags then
			for i, flag in ipairs(typ.flags) do
				local attrs = assertf(resolve_flag(schema, tbl, fld, flag))
				merge(fld, attrs)
			end
		end
		fld.is_type = nil
		fld.flags = nil
	end
end
local function parse_table(schema, name, t)
	local tbl = {is_table = true, name = name, fields = {}}
	add_cols(schema, tbl, t)
	function tbl.add_cols(t)
		add_cols(schema, tbl, t)
	end
	return tbl
end

local function parse_proc(schema, name, t)
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

local function add_fk(tbl, cols, ref_tbl, ondelete, onupdate)
	local fks = attr(tbl, 'fks')
	local k = _('fk_%s__%s', tbl.name, cat(cols, '_'))
	assertf(not fks[k], 'duplicate fk `%s`', k)
	ref_tbl = ref_tbl or assert(#cols == 1 and cols[1])
	fks[k] = {name = k, cols = cols,
		ref_table = ref_tbl, ondelete = ondelete, onupdate = onupdate}
	attr(tbl, 'deps')[ref_tbl] = true
end

local function add_ix(T, tbl, cols)
	local t = attr(tbl, T..'s')
	local k = _('%s_%s__%s', T, tbl.name, cat(cols, '_'))
	assertf(not t[k], 'duplicate %s `%s`', T, k)
	t[k] = {['is_'..T] = true, name = k, cols = cols}
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
		local env = update({}, schema.env)
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
		function env.add_proc    (...) self:add_proc    (...) end
		function env.add_trigger (...) self:add_trigger (...) end

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
			src = require(src)
		end
		if isfunc(src) then --def
			self:def(src)
		elseif isschema(src) then --schema
			if not self.loaded[src] then
				import(self, 'flags' , sc)
				import(self, 'types' , sc)
				import(self, 'tables', sc)
				self.loaded[src] = true
			end
		else
			assert(false)
		end
		return self
	end
end

function schema:def(f)
	if not self.loaded[f] then
		setfenv(f, self.env)
		f()
		self.loaded[f] = true
	end
	return self
end

schema.env = {}
schema.flags = {}

function schema.env.enum(...)
	local vals = names(cat({...}, ' '))
	return {is_type = true, isenum = true, enum_values = vals}
end

do
	local function fk_func(force_onupdate)
		return function(arg1, ...)
			if isschema(arg1) then --used as flag.
				local sch, tbl, fld = arg1, ...
				add_fk(tbl, {fld.name}, nil, force_onupdate)
			else --called by user, return a flag generator.
				local ref_tbl, ondelete, onupdate = arg1, ...
				return function(sch, tbl, fld)
					add_fk(tbl, {fld.name}, ref_tbl, ondelete, force_onupdate or onupdate)
				end
			end
		end
	end
	schema.env.fk       = fk_func()
	schema.env.child_fk = fk_func'cascade'
end

function schema:add_fk(tbl, cols, ...)
	local tbl = assertf(self.tables[tbl], 'unknown table `%s`', tbl)
	add_fk(tbl, names(cols), ...)
end
function schema:add_fk2(tbl, cols, ref_tbl, ondelete, onupdate)
	return self:add_fk(tbl, cols, ondelete, onupdate, '2')
end

do
	local function ix_func(T)
		return function(arg1, ...)
			if isschema(arg1) then --used as flag.
				local sch, tbl, fld = arg1, ...
				add_ix(T, tbl, {fld.name})
				fld[T] = true
			else --called by user, return a flag generator.
				local cols = names(cat({arg1, ...}, ' '))
				return function(sch, tbl, fld)
					if #cols == 0 then cols = {fld.name} end
					add_ix(T, tbl, cols)
				end
			end
		end
	end
	schema.env.uk = ix_func'uk'
	schema.env.ix = ix_func'ix'
end

function schema.flags.pk(sch, tbl, fld)
	assertf(not tbl.pk, 'pk already applied for table `%s`', tbl.name)
	tbl.pk = imap(tbl.fields, 'name')
	if sch.flags.not_null then
		for _,fld in ipairs(tbl.fields) do
			merge(fld, resolve_flag(sch, tbl, fld, 'not_null'))
		end
	end
end

function schema.env.default(v)
	return {default = v}
end

function schema.env.check(name)
	return function(code)
		return {ischeck = true, name = 'ck_'..name, code = code}
	end
end

function schema:add_proc(...)
	--TODO:
end

function schema:add_trigger(...)
	--TODO:
end

function schema:add_cols(...)
	--TODO:
end

schema.env.null = function() end

--dependency order -----------------------------------------------------------

local function dependency_order(items, item_deps)
	local function dep_maps()
		local t = {} --{item->{dep_item->true}
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

--schema diff'ing ------------------------------------------------------------

local function diff(t1, t2, diff_vals) --update t2 with differences from t1.
	local dt = {}
	for k,v1 in sortedpairs(t1) do
		local v2 = t2[k]
		if v2 == nil then
			add(dt, {'add', k, v1})
		elseif cmp then
			local vdt = diff_vals(v1, v2)
			if vdt then
				add(dt, {'update', k, vdt})
			end
		end
	end
	for k,v2 in sortedpairs(t2) do
		local v1 = t1[k]
		if v1 == nil then
			add(dt, {'remove', k})
		end
	end
	return #dt > 0 and dt or nil
end

function schema:diff_tables(t1, t2, renames)
	renames = renames or empty
	local dt = {}
	local function diff_fields(f1, f2)

	end
	local fdt = diff(t1.fields, t2.fields, diff_fields)
	if fdt then
		add(dt, {'fields', fdt})
	end
	if cat(t1.pk, '\0') ~= cat(t2.pk, '\0') then
		add(dt, {'pk', t1.pk})
	end
	local ukt = diff(t1.uks, t2.uks); if ukt then add(dt, {'uks', ukt}) end
	local fkt = diff(t1.fks, t2.fks); if fkt then add(dt, {'fks', fkt}) end
	local ixt = diff(t1.ixs, t2.ixs); if ixt then add(dt, {'ixs', fkt}) end
	return #dt > 0 and dt or nil
end

function schema:diff(sc2, opt)
	sc1 = self
	sc2 = sc2 or schema.new()
	local dt = {is_diff = true}
	local function diff_table(t1, t2)
		--return self:diff_tables(t1, t2, opt.table_renames)
	end
	dt.tables = diff(sc1.tables, sc2.tables, diff_table)
	local function diff_proc(p1, p2)
		--
	end
	dt.procs = diff(sc1.procs, sc2.procs, diff_proc)
	return dt
end

return schema
