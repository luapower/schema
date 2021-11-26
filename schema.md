
## `local schema = require'schema'`

This module implements a Lua-based definition language for RDBMS schemas.
Lua-based means that instead of parsing SQL to an AST, we make a small DSL
based on Lua's syntax and generate an AST with setfenv and metamethod magic
with no parsing. Lua may not be a homoiconic language to let us write ASTs
directly in it, but it gets pretty close.

So why would you want to declare SQL schemas in Lua anyway?
Here's some reasons:

* you want to generate SQL CREATE TABLE scripts for different SQL dialects
from a common structured format.
* you want to load RDBMS metadata into a common structured format
and report differences from a master schema and generate schema migrations.
* you want to annotate table fields with extra information for use in
data-bound widget toolkits like [x-widgets].

