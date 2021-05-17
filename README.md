Erlang simple string
====================

A wrapper library that attempts to make Erlang's string functions in the string, lists, erlang, and (eventually) re modules seem coherent to beginners.

The initial version is purely a wrapper for traditional list-based strings.  Future versions may:

* rename some functions to make them more familiar to developers coming from other languages
* fill some gaps for string functionality
* add support for binary strings across the set

The library is under the (2-clause) Simplified BSD license.

### How to generate documentation
1. Lounch command in project directory: `mkdir doc &&  cp -f overview.edoc doc/overview.edoc`.
2. Start Erlang shell (erl/werl).
3. Run code `edoc:files(["sstr.erl"], [{dir, "doc"}]). q().`.
4. Open file index.html in the `doc` subfolder.
