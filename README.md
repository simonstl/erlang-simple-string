# Erlang simple string


A wrapper library that attempts to make Erlang's string functions in the [string](http://erlang.org/doc/man/string.html), [lists](http://erlang.org/doc/man/lists.html), [erlang](http://erlang.org/doc/man/erlang.html), and [re](http://erlang.org/doc/man/re.html) modules seem coherent to beginners.

The initial version is purely a wrapper for traditional list-based strings.  Future versions may:

* rename some functions to make them more familiar to developers coming from other languages
* fill some gaps for string functionality
* add support for binary strings across the set

The library is under the (2-clause) Simplified BSD license.

## How to generate documentation
1. Lounch command in project directory: `mkdir doc &&  cp -f overview.edoc doc/overview.edoc`.
2. Start the shell (erl/werl).
3. Lounch the code `edoc:files(["sstr.erl"], [{dir, "doc"}]).` in the shell.
4. Open file index.html in the `doc` subfolder.

## How to run tests
1. Start the shell (erl/werl).
2. Lounch code `c(sstr). c(sstr_tests). eunit:test(sstr).` in the shell and analyze the results.

