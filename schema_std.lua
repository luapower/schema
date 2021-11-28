
--schema standard library.
--Written by Cosmin Apreutesei. Public Domain.

require'schema'

return function()

	flags.not_null = {not_null = true}
	flags.autoinc  = {autoinc = true}
	flags.unsigned = {unsigned = true}
	flags.ascii    = {charset = ascii}
	flags.bin      = {collate = bin}

	types.int      = {size = 4, mysql = 'int'}
	types.int8     = {int, size = 1, mysql = 'tinyint'}
	types.int16    = {int, size = 2, mysql = 'smallint'}
	types.int64    = {int, size = 8, mysql = 'bigint'}
	types.double   = {mysql = 'double'}
	types.bool     = {mysql = 'tinyint(1)'}
	types.str      = {mysql = 'varchar({maxlen})'}
	types.chr      = {mysql = 'char({len})'}
	types.text     = {mysql = 'text'}
	types.time     = {mysql = 'timestamp'}
	types.date     = {mysql = 'date'}
	types.dec      = {mysql = 'decimal({digits},{precision})'}
	types.mediumblob = {mysql = 'mediumblob'}

	types.bool0    = {bool, not_null, default = false}
	types.bool1    = {bool, not_null, default = true}
	types.id       = {int  , unsigned}
	types.pk       = {id   , pk, autoinc}
	types.bigid    = {int64, unsigned}
	types.bigpk    = {bigid, pk, autoinc}
	types.name     = {str  , maxlen =   64}
	types.strid    = {str  , maxlen =   64, ascii}
	types.strpk    = {strid, pk}
	types.email    = {str  , maxlen =  128, }
	types.hash     = {str  , maxlen =   64, ascii, bin} --enough for tohex(hmac.sha256())
	types.url      = {str  , maxlen = 2048, ascii}
	types.b64key   = {str  , maxlen = 8192, ascii, bin}
	types.atime    = {time , not_null, default = now}
	types.ctime    = {time , not_null, default = now}
	types.mtime    = {time , not_null, default = now, onupdate = now}
	types.money    = {dec  , digits = 15, precision = 3} -- 999 999 999 999 . 999      (fits in a double)
	types.qty      = {dec  , digits = 15, precision = 6} --     999 999 999 . 999 999  (fits in a double)
	types.percent  = {dec  , digits =  8, precision = 2} --         999 999 . 99
	types.count    = {int  , unsigned, not_null, default = 0}
	types.pos      = {int  , unsigned}
	types.lang     = {chr  , len = 2, ascii}
	types.currency = {chr  , len = 3, ascii}
	types.country  = {chr  , len = 2, ascii}

end
