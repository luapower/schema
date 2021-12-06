
--schema standard library.
--Written by Cosmin Apreutesei. Public Domain.

if not ... then require'schema_test'; return end

local M = {}
do

	local schema = require'schema'
	local glue = require'glue'
	local cat = table.concat
	local names = glue.names
	local format = string.format
	local outdent = glue.outdent

	function M.enum(...) --mysql-specific `enum` type
		local vals = names(cat({...}, ' '))
		return {is_type = true, type = 'enum', mysql_type = 'enum', enum_values = vals,
			charset = 'ascii', collation = 'ascii_bin' , mysql_collation = 'ascii_bin'}
	end

	function M.set(...) --mysql-specific `set` type
		local vals = names(cat({...}, ' '))
		return {is_type = true, type = 'set', mysql_type = 'set', set_values = vals,
			charset = 'ascii', collation = 'ascii_bin' , mysql_collation = 'ascii_bin'}
	end

	function M.mysql(s)
		return {mysql_body = _('begin\n%s\nend', outdent(outdent(s):trim(), '\t'))}
	end

	function M.bool_to_lua(v)
		if v == nil then return nil end
		return v ~= 0
	end

	function M.date_to_sql(v)
		if type(v) == 'number' then --timestamp
			return format('from_unixtime(%0.17g)', v)
		end
		return v
	end

	function M.lower_type(self, typ)
		if typ.default ~= nil and typ.mysql_default == nil then
			typ.mysql_default = typ.default
		end
	end

	--function M.default(self,

end

return function()

	import(M)

	flags.not_null   = {not_null = true}
	flags.autoinc    = {auto_increment = true}
	flags.ascii_ci   = {charset = ascii, collation = 'ascii_ci'  , mysql_charset = 'ascii'  , mysql_collation = 'ascii_general_ci'}
	flags.ascii_bin  = {charset = ascii, collation = 'ascii_bin' , mysql_charset = 'ascii'  , mysql_collation = 'ascii_bin'}
	flags.utf8_ci    = {charset = utf8 , collation = 'utf8_ci'   , mysql_charset = 'utf8mb4', mysql_collation = 'utf8mb4_0900_as_ci'}
	flags.utf8_ai_ci = {charset = utf8 , collation = 'utf8_ai_ci', mysql_charset = 'utf8mb4', mysql_collation = 'utf8mb4_0900_ai_ci'}
	flags.utf8_bin   = {charset = utf8 , collation = 'utf8_bin'  , mysql_charset = 'utf8mb4', mysql_collation = 'utf8mb4_0900_bin'}

	types.bool      = {type = 'bool', mysql_type = 'tinyint', size = 1, unsigned = true, decimals = 0, to_lua = bool_to_lua}
	types.bool0     = {bool , not_null, default = false, mysql_default = '0'}
	types.bool1     = {bool , not_null, default = true , mysql_default = '1'}
	types.int8      = {type = 'number', size = 1, decimals = 0, mysql_type = 'tinyint'  , min = -(2^ 7-1), max = 2^ 7}
	types.int16     = {type = 'number', size = 2, decimals = 0, mysql_type = 'smallint' , min = -(2^15-1), max = 2^15}
	types.int       = {type = 'number', size = 4, decimals = 0, mysql_type = 'int'      , min = -(2^31-1), max = 2^31}
	types.int52     = {type = 'number', size = 8, decimals = 0, mysql_type = 'bigint'   , min = -(2^52-1), max = 2^51}
	types.uint8     = {int8 , unsigned = true, min = 0, max = 2^ 8-1}
	types.uint16    = {int16, unsigned = true, min = 0, max = 2^16-1}
	types.uint      = {int  , unsigned = true, min = 0, max = 2^32-1}
	types.uint52    = {int52, unsigned = true, min = 0, max = 2^52-1}
	types.double    = {type = 'number' , size = 8, mysql_type = 'double'}
	types.float     = {type = 'number' , size = 4, mysql_type = 'float'}
	types.dec       = {type = 'decimal', mysql_type = 'decimal'}
	types.str       = {type = 'text', mysql_type = 'varchar'}
	types.text      = {str, mysql_type = 'text', size = 0xffff, maxlen = 0xffff, utf8_bin}
	types.chr       = {str, mysql_type = 'char', padded = true}
	types.blob      = {mysql_type = 'mediumblob', size = 0xffffff}
	types.time      = {type = 'time', mysql_type = 'bigint'}
	types.timeofday = {type = 'timeofday', mysql_type = 'time'}
	types.date      = {type = 'date', mysql_type = 'date', to_sql = date_to_sql}
	types.datetime  = {type = 'date', has_time = true, mysql_type = 'datetime'}
	types.timestamp = {datetime, mysql_type = 'timestamp'}

	types.id        = {uint}
	types.idpk      = {id, pk, autoinc}
	types.bigid     = {uint52}
	types.bigidpk   = {bigid, pk, autoinc}

	types.name      = {str, size = 256, maxlen = 64, utf8_ai_ci}
	types.strid     = {str, size =  64, maxlen = 64, ascii_bin}
	types.strpk     = {strid, pk}
	types.email     = {str, size =  512, maxlen =  128, utf8_ci}
	types.hash      = {str, size =   64, maxlen =   64, ascii_bin} --enough for tohex(hmac.sha256())
	types.url       = {str, size = 2048, maxlen = 2048, ascii_bin}
	types.b64key    = {str, size = 8192, maxlen = 8192, ascii_bin}

	types.atime     = {datetime, not_null, default = '', mysql_default = 'current_timestamp'}
	types.ctime     = {datetime, not_null, default = '', mysql_default = 'current_timestamp'}
	types.mtime     = {datetime, not_null, default = '', mysql_default = 'current_timestamp', mysql_on_update = 'current_timestamp'}

	types.money     = {dec, digits = 15, decimals = 3} -- 999 999 999 999 . 999     (fits in a double)
	types.qty       = {dec, digits = 15, decimals = 6} --     999 999 999 . 999 999 (fits in a double)
	types.percent   = {dec, digits =  8, decimals = 2} --         999 999 . 99
	types.count     = {uint, not_null, default = 0, mysql_default = '0'}
	types.pos       = {uint}

	types.lang      = {chr, size = 2, maxlen = 2, ascii_bin}
	types.currency  = {chr, size = 3, maxlen = 3, ascii_bin}
	types.country   = {chr, size = 2, maxlen = 2, ascii_bin}

end
