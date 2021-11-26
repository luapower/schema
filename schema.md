
## `local schema = require'schema'`

So how do you keep your database schema definition that you need to apply
to the database server on a fresh install of your application? In SQL files
containing CREATE TABLE statements? I have a better idea.

This module implements a Lua-based Data Definition Language (DDL) for RDBMS
schemas. Lua-based means that instead of a textual format like SQL DDL,
we use Lua syntax to write table definitions in, and generate an Abstract
Syntax Three (AST) from that. Using setfenv and metamethod magic we create
a language that is very readable and at the same time more expressive than
any textual format could be, giving us full programming power in an otherwise
declarative language. Basically a metaprogrammed DDL.

So why would you want to keep your database schema in the application anyway?
Here's some reasons:

* you want to generate SQL DDL scripts for different SQL dialects
from a common structured format.
* you want to diff between a live database and a master schema to find out 
if the database is migrated properly.
* you want to generate schema migrations (semi-)automatically.
* you want to annotate table fields with extra information for use in
data-bound widget toolkits like [x-widgets], and you don't want to do that
off-band in a separate file.
* you want a "shell" API for bulk DML ops like copying tables between 
different databases with different engines.
* use it as a base for a scriptable ETL tool.

## API

<warn>WIP</warn>

--------------------------------- -------------------------------------------
`schema.new(opt) -> sc`           create a new schema object
--------------------------------- -------------------------------------------
