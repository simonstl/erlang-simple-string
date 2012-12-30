-module(sstr).
-export([len/1, length/1, concat/1, concat/2, append/1, append/2, nth/2, hd/1, chr/2, str/2, substr/2, substr/3, sub_string/2, sub_string/3, tokens/2, join/2, words/1, words/2, chars/2, chars/3, copies/2, strip/1, strip/2, strip/3, left/2, left/3, right/2, right/3, centre/2, centre/3, reverse/1, reverse/2, to_float/1, to_integer/1, to_lower/1, to_upper/1, integer_to_list/1, integer_to_list/2, float_to_list/1, fun_to_list/1, list_to_atom/1]).

len(Arg1) -> string:len(Arg1).

length(Arg1) -> erlang:length(Arg1).

concat(Arg1) -> string:concat(Arg1).
concat(Arg1, Arg2) -> string:concat(Arg1, Arg2).

append(Arg1) -> lists:append(Arg1).
append(Arg1, Arg2) -> lists:append(Arg1, Arg2).

nth(Arg1, Arg2) -> lists:nth(Arg1, Arg2).

hd(Arg1) -> erlang:hd(Arg1).

chr(Arg1, Arg2) -> string:chr(Arg1, Arg2).

str(Arg1, Arg2) -> string:str(Arg1, Arg2).

substr(Arg1, Arg2) -> string:substr(Arg1, Arg2).
substr(Arg1, Arg2, Arg3) -> string:substr(Arg1, Arg2, Arg3).

sub_string(Arg1, Arg2) -> string:sub_string(Arg1, Arg2).
sub_string(Arg1, Arg2, Arg3) -> string:sub_string(Arg1, Arg2, Arg3).

tokens(Arg1, Arg2) -> string:tokens(Arg1, Arg2).

join(Arg1, Arg2) -> string:join(Arg1, Arg2).

words(Arg1) -> string:words(Arg1).
words(Arg1, Arg2) -> string:words(Arg1, Arg2).

chars(Arg1, Arg2) -> string:chars(Arg1, Arg2).
chars(Arg1, Arg2, Arg3) -> string:chars(Arg1, Arg2, Arg3).

copies(Arg1, Arg2) -> string:copies(Arg1, Arg2).

strip(Arg1) -> string:strip(Arg1).
strip(Arg1, Arg2) -> string:strip(Arg1, Arg2).
strip(Arg1, Arg2, Arg3) -> string:strip(Arg1, Arg2, Arg3).

left(Arg1, Arg2) -> string:left(Arg1, Arg2).
left(Arg1, Arg2, Arg3) -> string:left(Arg1, Arg2, Arg3).

right(Arg1, Arg2) -> string:right(Arg1, Arg2).
right(Arg1, Arg2, Arg3) -> string:right(Arg1, Arg2, Arg3).

centre(Arg1, Arg2) -> string:centre(Arg1, Arg2).
centre(Arg1, Arg2, Arg3) -> string:centre(Arg1, Arg2, Arg3).

reverse(Arg1) -> lists:reverse(Arg1).
reverse(Arg1, Arg2) -> lists:reverse(Arg1, Arg2).

to_float(Arg1) -> string:to_float(Arg1).

to_integer(Arg1) -> string:to_integer(Arg1).

to_lower(Arg1) -> string:to_lower(Arg1).

to_upper(Arg1) -> string:to_upper(Arg1).

integer_to_list(Arg1) -> erlang:integer_to_list(Arg1).
integer_to_list(Arg1, Arg2) -> erlang:integer_to_list(Arg1, Arg2).

float_to_list(Arg1) -> erlang:float_to_list(Arg1).

fun_to_list(Arg1) -> erlang:fun_to_list(Arg1).

list_to_atom(Arg1) -> erlang:list_to_atom(Arg1).