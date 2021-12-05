
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

	--local cn = spp.connect{fake = true}
	--local diff = sc:diff()
	--print(cat(cn:sqldiff(diff), ';\n\n'))

	if true then

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

		local diff = sc:diff(dbsc) --schema.new{engine = 'mysql'})
		diff:pp{
			hide_attrs = {mysql_collation=1, mysql_default=1},
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
