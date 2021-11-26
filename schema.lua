
--RDBMS schema definition language & operations
--Written by Cosmin Apreutesei. Public Domain.

local glue = require'glue'
require'$'

--definition parsing ---------------------------------------------------------

local schema = {isschema = true}

local function set_flag(flags, name, attrs)
	assertf(not flags[name], 'duplicate flag: %s', name)
	rawset(flags, name, attrs)
end

local function set_type(types, name, t)
	assertf(not types._[name], 'duplicate type: %s', name)
	local super = t[1]
	if isstr(super) then
		super = assertf(types[super], 'unknown super type `%s` for type `%s`', super, name)
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
	types._[name] = typ
	return typ
end

local function apply_flag(sch, tbl, fld, flag)
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

local function set_table(tables, name, t)
	assertf(not tables._[name], 'duplicate table: %s', name)
	local tbl = {istable = true, name = name, fields = {}}
	local i = 1
	while i <= #t do --field_name, field_type, field_flag|field_attrs, ...
		local col = t[i]
		assertf(isstr(col), 'column name expected for `%s`, got %s', name, type(col))
		i = i + 1
		local typ = t[i]
		if isstr(typ) then
			typ = assertf(tables.schema.types[typ], 'unknown type for `%s.%s`', name, col)
		else
			assertf(istab(typ) and typ.istype, 'type for `%s.%s` is not a type', name, col)
		end
		i = i + 1
		local fld = merge({name = col, isfield = true}, typ)
		add(tbl.fields, fld)
		while i <= #t do
			local flag = t[i]
			local attrs, err = apply_flag(tables.schema, tbl, fld, flag)
			if not attrs then --next field or error
				break
			end
			update(fld, attrs)
			i = i + 1
		end
		--apply flag generators from types after the more specific ones in the field.
		if typ.flags then
			for i, flag in ipairs(typ.flags) do
				local attrs = assertf(apply_flag(tables.schema, tbl, fld, flag))
				merge(fld, attrs)
			end
		end
		fld.istype = nil
		fld.flags = nil
	end
	tables._[name] = tbl
	return tbl
end

schema.env = {}

local function get_(t, k) return t._[k] end

function schema.new(opt)
	local self = object(schema, opt)
	self.env = object(schema.env)
	self.flags = object(nil, {_ = update({}, schema.flags), schema = self})
	self.flags.__index    = get_
	self.flags.__newindex = set_flag
	self.types = object(nil, {_ = update({}, schema.types), schema = self})
	self.types.__index    = get_
	self.types.__newindex = set_type
	self.tables = object(nil, {_ = update({}, schema.tables), schema = self})
	self.tables.__index    = get_
	self.tables.__newindex = set_table
	self.env.flags  = self.flags
	self.env.types  = self.types
	self.env.tables = self.tables
	return self
end

function schema:def(f)
	local function get_global(t, k)
		if self.env[k] ~= nil then
			return self.env[k]
		end
		rawset(t, k, k)
		return k
	end
	local env = setmetatable({}, {__index = get_global})
	setfenv(f, env)
	f()
end

function schema.env.enum(s, ...)
	local t = isstr(s) and names(s) or {s, ...}
	return {istype = true, isenum = true, enum_values = t}
end

function schema.env.fk(ref_tbl, ondelete, onupdate)
	return function(sch, tbl, fld)
		local fks = attr(tbl, 'fks')
		local name = _('fk_%s_%s', tbl.name, fld.name)
		assertf(not fks[name], 'duplicate foreign key `%s`', name)
		add(fks, {name = name, isfk = true, fields = {fld.name},
			ref_table = ref_tbl, ondelete = ondelete, onupdate = onupdate})
		return empty
	end
end

schema.flags = {}

function schema.flags.pk(sch, tbl, fld)
	assertf(not tbl.pk, 'pk already applied for table `%s`', tbl.name)
	tbl.pk = imap(tbl.fields, 'name')
	if #tbl.fields == 1 then
		tbl.fields[1].pk = true
	end
	return empty
end

--SQL formatting -------------------------------------------------------------

schema.dialects = {mysql = {}, tarantool = {}}

function schema.dialects.mysql:sqlname(s)
	return s
end

function schema.dialects.mysql:sqltype(fld)
	local s = subst(fld.mysql, fld)
	return _('%s%s%s', s,
		fld.unsigned and ' unsigned' or '',
		fld.pk and ' primary key' or '')
end

function schema:sql(o, dialect_name)
	dialect_name = dialect_name or self.dialect
	local dialect = assert(self.dialects[dialect_name])
	local function sqlname(s) return dialect:sqlname(s) end
	local function cols(t) return concat(imap(t, sqlname), ', ') end
	if o.istable then

		local t = {}
		for i,fld in ipairs(o.fields) do
			local s = self:sql(fld, nil, dialect_name)
			add(t, s)
		end
		local fields = concat(t, ',\n\t')

		local pk = o.pk and #o.pk > 1 and _(',\n\tprimary key (%s)', cols(o.pk))

		local fks
		if o.fks then
			fks = {}
			for i,fk in ipairs(o.fks) do
				local ondelete = fk.ondelete or 'restrict'
				local onupdate = fk.onupdate or 'cascade'
				local a1 = ondelete ~= 'restrict' and ' on delete '..ondelete or ''
				local a2 = onupdate ~= 'restrict' and ' on update '..onupdate or ''
				local ref_cols = cols(self.tables[fk.ref_table].pk)
				local s = fmt(',\n\tconstraint %s foreign key (%s) references %s (%s)%s%s',
					fk.name, cols(fk.fields), sqlname(fk.ref_table), ref_cols, a1, a2)
				add(fks, s)
			end
		end
		fks = fks and concat(fks, ',\n\t') or ''

		return _('%s (\n\t%s%s%s\n)', o.name, fields, pk, fks)
	elseif o.isfield then
		return _('%-16s %s', dialect:sqlname(o.name), dialect:sqltype(o))
	else
		error'NYI'
	end
end

if not ... then --------------------------------------------------------------

local sc = schema.new{dialect = 'mysql'}

sc:def(function()

	flags.not_null = {not_null = true}
	flags.autoinc  = {autoinc = true}
	flags.unsigned = {unsigned = true}
	flags.ascii    = {charset = ascii}
	flags.bin      = {collate = bin}

	types.int      = {mysql = 'int'}
	types.booln    = {mysql = 'tinyint(1)'}
	types.int64    = {mysql = 'bigint'}
	types.str      = {mysql = 'varchar({maxlen})'}
	types.text     = {mysql = 'text'}
	types.time     = {mysql = 'timestamp'}
	types.date     = {mysql = 'date'}
	types.dec      = {mysql = 'decimal({digits},{precision})'}

	types.id       = {int  , unsigned}
	types.pk       = {id   , pk, autoinc}
	types.bigid    = {int64, unsigned}
	types.bigpk    = {bigid, pk, autoinc}
	types.name     = {str  , maxlen =   64}
	types.strid    = {str  , maxlen =   64, charset = ascii}
	types.strpk    = {strid, pk}
	types.email    = {str  , maxlen =  128, }
	types.hash     = {str  , maxlen =   64, charset = ascii, collate = bin} --enough for tohex(hmac.sha256())
	types.url      = {str  , maxlen = 2048, charset = ascii}
	types.b64key   = {str  , maxlen = 8192, charset = ascii, collate = bin}
	types.bool     = {booln, not_null, default = false}
	types.bool1    = {booln, not_null, default = true}
	types.atime    = {time , not_null, default = now}
	types.ctime    = {time , not_null, default = now}
	types.mtime    = {time , not_null, default = now, onupdate = now}
	types.money    = {dec  , digits = 15, precision = 3} -- 999 999 999 999 . 999      (fits in a double)
	types.qty      = {dec  , digits = 15, precision = 6} --     999 999 999 . 999 999  (fits in a double)
	types.percent  = {dec  , digits =  8, precision = 2} --         999 999 . 99
	types.count    = {int  , unsigned, not_null, default = 0}
	types.pos      = {int  , unsigned}
	types.lang     = {str  , len = 2, charset = ascii}
	types.currency = {str  , len = 3, charset = ascii}
	types.country  = {str  , len = 2, charset = ascii}

end)

sc:def(function()

	tables.usr = {
		usr         , pk      ,
		anonymous   , bool1   ,
		email       , email   ,
		emailvalid  , bool    ,
		pass        , hash    ,
		facebookid  , name    ,
		googleid    , name    ,
		gimgurl     , url     , --google image url
		active      , bool1   ,
		title       , name    ,
		name        , name    ,
		phone       , name    ,
		phonevalid  , bool    ,
		sex         , enum'M F',
		birthday    , date    ,
		newsletter  , bool    ,
		roles       , text    ,
		note        , text    ,
		clientip    , name    , --when it was created
		atime       , atime   , --last access time
		ctime       , ctime   , --creation time
		mtime       , mtime   , --last modification time
	}

	tables.session = {
		token       , hash   , not_null, pk,
		usr         , id     , not_null, fk(usr, cascade),
		expires     , time   , not_null,
		clientip    , name   , --when it was created
		ctime       , ctime  ,
	}

	tables.usrtoken = {
		token       , hash   , not_null, pk,
		usr         , id     , not_null, fk(usr),
		expires     , time   , not_null,
		validates   , enum'email phone', not_null,
		ctime       , ctime  ,
	}

end)

print(sc:sql(sc.tables.session))

end

return schema
