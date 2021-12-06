
local function sc1()

	import'schema_std'

	tables.t0 = {
		f1, id, pk,
	}

	tables.t1 = {
		f1, idpk, ix, fk(t0),
		f2, name, uk, check 'a < b',
	}

	trigger(foo, after, insert, t1, mysql [[
		dude wasup;
	]])

end

local function sc2()

	import'schema_std'

	tables.t1 = {
		f1, id,
		f2, bigid, pk, check 'a <= b', ix,
		f3, name, uk, fk(t1),
	}

	tables.t2 = {
		f1, idpk, uk, ix, check 'a > b',
	}

	trigger(foo, after, insert, t2, mysql [[
		dude wasup again;
	]])

	proc(foobar, {int, out, uint8}, mysql [[
		foobar;
	]])

end

local schema = require'schema'
local spp = require'sqlpp'.new()
require'sqlpp_mysql'
spp.import'mysql'

local sc = schema.new()
require'webb_auth'
require'webb_lang'
local auth_schema = webb.auth_schema
local lang_schema = webb.lang_schema
local sp_schema = require'sp'.schema

local getfenv = getfenv
local pairs = pairs
local print = print

sc:import(function()
	import'schema_std'
	--for k, v in pairs(getfenv()) do print(k) end
	--types.int = {}
	--tables.t1 = {xx, int}

	import(auth_schema)
	import(lang_schema)
	import(sp_schema)

	tables.blah = {
		id, pk,
		name, name,
	}
end)

webb.run(function()

	if true then
		local rt = {
			digits=1,
			decimals=1,
			size=1, --not relevant for numbers, mysql_type is enough.
			maxlen=1,
			unsigned=1,
			not_null=1,
			auto_increment=1,
			comment=1,
			mysql_type=1,
			mysql_charset=1,
			mysql_collation=1,
			mysql_default=1,
		}
		local function rf()
			return rt
		end
		local st1 = {
			engine = 'mysql',
			relevant_field_attrs = rf,
			supports_fks = true,
			supports_checks = true,
			supports_triggers = true,
			supports_procs = true,
		}
		local st2 = update({}, st1)
		local sc1 = schema.new(st1):import(sc1)
		local sc2 = schema.new(st2):import(sc2)
		local d = sc2:diff(sc1)
		pp(d)
		d:pp()
	end

	--local cn = spp.connect{fake = true}
	--local diff = sc:diff()
	--print(cat(cn:sqldiff(diff), ';\n\n'))

	if false then

		local cn = assert(spp.connect{
			host = '127.0.0.1',
			port = 3307,
			user = 'root',
			password = 'root',
			db = 'sp',
			collation = 'server',
		})

		local dbsc = cn:extract_schema('sp')
		--pp(sc.tables.addr)
		--pp(sc.procs)

		local diff = sc:diff(nil, {engine = 'mysql'})
		diff:pp{
			--hide_attrs = {mysql_collation=1, mysql_default=1},
		}
		--pp(diff)
		--pp(diff.tables.update.addr.fields.update.note)
		--pp(diff.tables.update.addr.fields.remove.note)
		--local diff = sc:diff()
		--print(cn:sqldiff(diff)[1])
		--[[
		for s,d in pairs(diff.tables.update) do
			print(_('%-16s %s', s, d.fields and d.fields.update
				and pp.format(map(d.fields.update, function(col, fld)
				return cat(keys(fld.changed), ' ')
				end))))
		end
		]]
		--print(cat(cn:sqldiff(diff), ';\n'))

		--pp(sc:table_create_order())

	end


end)
