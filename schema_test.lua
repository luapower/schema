
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

sc:def(function()
	import'schema_std'
	--for k, v in pairs(getfenv()) do print(k) end
	--types.int = {}
	--tables.t1 = {xx, int}

	import(auth_schema)
	import(lang_schema)
	import(sp_schema)
end)

webb.run(function()

	if false then

		local cn = spp.connect{fake = true}
		--require'inspect'
		local diff = sc:diff()
		--pp(diff)

		print(cat(cn:sqldiff(diff), ';\n\n'))

	end

	if true then

		local cn = assert(spp.connect{
			host = '127.0.0.1',
			port = 3307,
			user = 'root',
			password = 'root',
			schema = 'sp',
			collation = 'server',
		})

		local sc = cn:extract_schema('sp.*')

		local diff = sc:diff()
		print(cat(cn:sqldiff(diff), ';\n\n'))


	end


end)
