
--schema standard library.
--Written by Cosmin Apreutesei. Public Domain.

require'schema'

return function()

	flags.not_null  = {not_null = true}
	flags.autoinc   = {auto_increment = true}
	flags.unsigned  = {unsigned = true}

	flags.ascii_ci  = {charset = ascii, case_insensitive = true, mysql_collation = 'ascii_general_ci'}
	flags.ascii_bin = {charset = ascii, mysql_collation = 'ascii_bin'}
	flags.utf8      = {charset = utf8, case_insensitive = true, accent_insensitive = true, mysql_collation = 'utf8mb4_0900_ai_ci'}
	flags.utf8_bin  = {charset = utf8, mysql_collation = 'utf8mb4_0900_bin'}

	types.int8      = {size = 1, mysql_type = 'tinyint'  , decimals = 0, min = -(2^ 7-1), max = 2^ 7}
	types.int16     = {size = 2, mysql_type = 'smallint' , decimals = 0, min = -(2^15-1), max = 2^15}
	types.int       = {size = 4, mysql_type = 'int'      , decimals = 0, min = -(2^31-1), max = 2^31}
	types.int52     = {size = 8, mysql_type = 'bigint'   , decimals = 0, min = -(2^52-1), max = 2^51}

	types.uint8    = {int8 , unsigned, min = 0, max = 2^ 8-1}
	types.uint16   = {int16, unsigned, min = 0, max = 2^16-1}
	types.uint     = {int  , unsigned, min = 0, max = 2^32-1}
	types.uint52   = {int52, unsigned, min = 0, max = 2^52-1}

	types.double   = {mysql_type = 'double'}
	types.float    = {mysql_type = 'float'}

	types.bool     = {mysql_type = 'tinyint(1)'}



	types.str      = {mysql_type_subst = 'varchar({maxlen})'}
	types.text     = {str, mysql_type = 'text'}

	types.chr      = {type = 'text', my = 'char({len})'}

	types.binary   = {mysql_type_gen = function(t) return _('binary(%d)', n) end}

	types.bin      = {my = 'blob({len})'}


	types.time      = {mysql_type = 'bigint'}
	types.date      = {mysql_type = 'date'}
	types.localtime = {mysql_type = 'datetime'}
	types.dec       = {mysql_type = 'decimal({digits},{decimals})'}

	types.blob     = {mysql_type = 'mediumblob'}

	types.bool0    = {bool , not_null, default = false}
	types.bool1    = {bool , not_null, default = true}

	types.id       = {int  , unsigned}
	types.pk       = {id   , pk, autoinc}
	types.bigid    = {uint52}
	types.bigpk    = {bigid, pk, autoinc}

	types.name     = {str  , maxlen =   64}
	types.strid    = {str  , maxlen =   64, ascii_bin}
	types.strpk    = {strid, pk}
	types.email    = {str  , maxlen =  128, }
	types.hash     = {str  , maxlen =   64, ascii_bin} --enough for tohex(hmac.sha256())
	types.url      = {str  , maxlen = 2048, ascii_bin}
	types.b64key   = {str  , maxlen = 8192, ascii_bin}

	types.atime    = {time , not_null, mysql_default = 'current_timestamp'}
	types.ctime    = {time , not_null, mysql_default = 'current_timestamp'}
	types.mtime    = {time , not_null, mysql_default = 'current_timestamp', mysql_on_update = 'current_timestamp'}

	types.money    = {dec  , digits = 15, decimals = 3} -- 999 999 999 999 . 999     (fits in a double)
	types.qty      = {dec  , digits = 15, decimals = 6} --     999 999 999 . 999 999 (fits in a double)
	types.percent  = {dec  , digits =  8, decimals = 2} --         999 999 . 99

	types.count    = {uint , not_null, default = 0}
	types.pos      = {uint}

	types.lang     = {chr, len = 2, ascii_bin}
	types.currency = {chr, len = 3, ascii_bin}
	types.country  = {chr, len = 2, ascii_bin}

end
