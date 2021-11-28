
--RDBMS schema definition language & operations
--Written by Cosmin Apreutesei. Public Domain.

if not ... then require'schema_test'; return end

local glue = require'glue'
require'$'

--definition parsing ---------------------------------------------------------

local schema = {isschema = true}

local function isschema(t)
	return istab(t) and t.isschema
end

local function parse_type(schema, name, t)
	local super = t[1]
	if isstr(super) then
		super = assertf(schema.types[super], 'unknown super type `%s` for type `%s`', super, name)
	elseif super then
		assertf(istab(super) and super.istype, 'super type for type `%s` is not a type', name)
	end
	local flags = extend({}, super and super.flags)
	append(flags, unpack(t, 2))
	local typ = merge({istype = true, ['is'..name] = true, flags = flags}, super)
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
			assertf(istab(typ) and typ.istype, 'type for `%s.%s` is not a type', name, col)
		end
		i = i + 1
		local fld = merge({isfield = true, name = col}, typ)
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
		fld.istype = nil
		fld.flags = nil
	end
end
local function parse_table(schema, name, t)
	local tbl = {istable = true, name = name, fields = {}}
	add_cols(schema, tbl, t)
	function tbl.add_cols(t)
		add_cols(schema, tbl, t)
	end
	return tbl
end

local function add_global(t, k, v)
	assertf(not t.flags [k], 'global overshadows flag `%s`', k)
	assertf(not t.types [k], 'global overshadows type `%s`', k)
	assertf(not t.tables[k], 'global overshadows table `%s`', k)
	rawset(t, k, v)
end

local function add_fk(tbl, cols, ref_tbl, ondelete, onupdate, suffix)
	local fks = attr(tbl, 'fks')
	local k = _('fk_%s_%s%s', tbl.name, cat(cols, '_'), suffix or '')
	assertf(not fks[k], 'duplicate fk `%s`', k)
	ref_tbl = ref_tbl or assert(#cols == 1 and cols[1])
	fks[k] = {isfk = true, name = k, cols = cols,
		ref_table = ref_tbl, ondelete = ondelete, onupdate = onupdate}
	attr(tbl, 'deps')[ref_tbl] = true
end

local function add_ix(T, tbl, cols)
	local t = attr(tbl, T..'s')
	local k = _('%s_%s_%s', T, tbl.name, cat(cols, '_'))
	assertf(not t[k], 'duplicate %s `%s`', T, k)
	t[k] = {['is'..T] = true, name = k, cols = cols}
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
		local self = update(opt or {}, schema)
		local env = update({}, schema.env)
		init(self, env, 'flags' , pass)
		init(self, env, 'types' , parse_type)
		init(self, env, 'tables', parse_table)
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
	local function import(k, sc, sc1)
		local k1 = k:gsub('s$', '')
		for k,v in pairs(sc1[k]) do
			assertf(not sc[k], 'duplicate %s `%s`', k1, k)
			sc[k] = v
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
				import('flags' , self, sc)
				import('types' , self, sc)
				import('tables', self, sc)
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

local function enum_mysql(dialect, fld)
	local function str(s) dialect:sqlstring(s) end
	return _('enum(%s)', cat(imap(fld.enum_values, str), ', '))
end
function schema.env.enum(...)
	local vals = names(cat({...}, ' '))
	return {istype = true, isenum = true, enum_values = vals, mysql = enum_mysql}
end

do
	local function fk_func(suffix, force_onupdate)
		return function(arg1, ...)
			if isschema(arg1) then --used as flag.
				local sch, tbl, fld = arg1, ...
				add_fk(tbl, {fld.name}, nil, force_onupdate, nil, suffix)
			else --called by user, return a flag generator.
				local ref_tbl, ondelete, onupdate = arg1, ...
				return function(sch, tbl, fld)
					add_fk(tbl, {fld.name}, ref_tbl, ondelete, force_onupdate or onupdate, suffix)
				end
			end
		end
	end
	schema.env.fk        = fk_func(nil)
	schema.env.fk2       = fk_func('2')
	schema.env.child_fk  = fk_func(nil, 'cascade')
	schema.env.child_fk2 = fk_func('2', 'cascade')
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
	if #tbl.fields == 1 then
		tbl.fields[1].pk = true
	end
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

--SQL formatting -------------------------------------------------------------

schema.dialects = {mysql = {}, tarantool = {}}

function schema.dialects.mysql:sqlname(s)
	return s
end

function schema.dialects.mysql:sqlstring(s)
	return "'"..s.."'"
end

function schema.dialects.mysql:sqltype(fld)
	assertf(fld.mysql, 'mysql property missing for field `%s`', fld.name)
	local s, missing
	if isfunc(fld.mysql) then
		s = fld.mysql(self, fld)
	else
		s, missing = subst(fld.mysql, fld, true)
		if missing then
			error(_('field `%s` is missing `%s`', fld.name, cat(missing, ', ')))
		end
	end
	return _('%s%s%s', s,
		fld.unsigned and ' unsigned' or '',
		fld.pk and ' primary key' or '')
end

function schema:sql(o, dialect_name)
	if not o then
		local t = {}
		local tbls, circular_deps = self:table_create_order()
		pp(circular_deps)
		for _,tbl in ipairs(tbls) do
			add(t, self:sql(self.tables[tbl], dialect_name))
		end
		return cat(t, ';\n')
	end
	dialect_name = dialect_name or self.dialect
	local dialect = assertf(self.dialects[dialect_name], 'unknown dialect `%s`', dialect_name)
	local function sqlname(s) return dialect:sqlname(s) end
	local function cols(t) return cat(imap(t, sqlname), ', ') end
	if o.istable then

		local t = {}
		for i,fld in ipairs(o.fields) do
			local s = self:sql(fld, nil, dialect_name)
			add(t, s)
		end
		local fields = cat(t, ',\n\t')

		local pk = o.pk and #o.pk > 1 and _(',\n\tprimary key (%s)', cols(o.pk)) or ''

		local fks
		if o.fks then
			fks = {}
			for _,fk in pairs(o.fks) do
				local ondelete = fk.ondelete or 'restrict'
				local onupdate = fk.onupdate or 'cascade'
				local a1 = ondelete ~= 'restrict' and ' on delete '..ondelete or ''
				local a2 = onupdate ~= 'restrict' and ' on update '..onupdate or ''
				local ref_tbl = assertf(self.tables[fk.ref_table], 'unknown ref table `%s`', fk.ref_table)
				local ref_cols = cols(ref_tbl.pk)
				local s = fmt('constraint %s foreign key (%s) references %s (%s)%s%s',
					fk.name, cols(fk.cols), sqlname(fk.ref_table), ref_cols, a1, a2)
				add(fks, s)
			end
			fks = ',\n\t'..cat(fks, ',\n\t') or ''
		end

		return _('%s (\n\t%s%s%s\n)', o.name, fields, pk, fks)
	elseif o.isfield then
		return _('%-16s %s', dialect:sqlname(o.name), dialect:sqltype(o))
	else
		error'NYI'
	end
end

--mysql schema loading -------------------------------------------------------

local std_type = {
	bigint      = 'int64',
	binary      = '',
	blob        = '',
	char        = '',
	date        = '',
	datetime    = '',
	decimal     = '',
	double      = '',
	enum        = '',
	float       = '',
	int         = '',
	json        = '',
	longblob    = '',
	longtext    = '',
	mediumblob  = '',
	mediumint   = '',
	mediumtext  = '',
	set         = '',
	smallint    = '',
	text        = '',
	time        = '',
	timestamp   = '',
	tinyint     = '',
	varbinary   = '',
	varchar     = '',
}

function schema:mysql_load(cn, schema)
	local row, err = cn:query(_([[
		select
			t.table_name tbl,
			c.column_name col,
			c.data_type,
		from
			information_schema.tables t
			inner join information_schema.columns c
				on c.table_schema = t.table_schema
				and c.table_name = t.table_name
		where
			t.table_schema = '%s'
	]], cn:esc(schema)))

	if not res then
		return nil, err
	end

	for _,row in ipairs(rows) do
		local tbl = self.tables[row.tbl]
		if not tbl then
			tbl = {istable = true, name = row.tbl, fields = {}}
			self.tables[row.tbl] = tbl
		end
		local typ = std_type[row.data_type]
		local typ = assertf(self.types[typ], 'no field type for mysql type `%s`', row.data_type)
		local fld = merge({
			isfield = true,
			name = row.col,
		}, typ)
		tbl.fields[row.col] = fld
	end
end

return schema
