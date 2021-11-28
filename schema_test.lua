
local schema = require'schema'
local sc = schema.new{dialect = 'mysql'}

require'webb_auth'
require'webb_lang'
local auth_schema = webb.auth_schema
local lang_schema = webb.lang_schema
local sp_schema = require'sp'.schema

sc:def(function()
	import'schema_std'
	import(auth_schema)
	import(lang_schema)
	import(sp_schema)
end)

pp(sc:sql())
