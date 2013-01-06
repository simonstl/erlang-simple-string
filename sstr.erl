-module(sstr).
-export([len/1, length/1, concat/1, concat/2, append/1, append/2, nth/2, hd/1]).
-export([chr/2, str/2, substr/2, substr/3, sub_string/2, sub_string/3, tokens/2]).
-export([join/2, words/1, words/2, chars/2, chars/3, copies/2, strip/1, strip/2]).
-export([strip/3, left/2, left/3, right/2, right/3, centre/2, centre/3, reverse/1]).
-export([reverse/2, to_float/1, to_integer/1, to_lower/1, to_upper/1]).
-export([integer_to_list/1, integer_to_list/2, float_to_list/1, fun_to_list/1]). 
-export([list_to_atom/1, equal/2, rchr/2, rstr/2, span/2, cspan/2]).

-spec(len(string()) -> integer()).
len(String) -> string:len(String).

-spec(equal(string(), string()) -> boolean()).
equal(String1, String2) -> string:equal(String1, String2).

-spec(length(string()) -> integer()).
length(String) -> erlang:length(String).

-spec(concat(list()) -> string()).
concat(ListOfStrings) -> lists:concat(ListOfStrings).

-spec(concat(string(), string()) -> string()).
concat(String1, String2) -> string:concat(String1, String2).

-spec(append(list()) -> string()).
append(ListOfStrings) -> lists:append(ListOfStrings).

-spec(append(string(), string()) -> string()).
append(String1, String2) -> lists:append(String1, String2).

-spec(nth(integer(), string()) -> char()).
nth(Position, String) -> lists:nth(Position, String).

-spec(hd(string()) -> char()).
hd(String) -> erlang:hd(String).

-spec(chr(string(), char()) -> integer()).
chr(String, CharToFind) -> string:chr(String, CharToFind).

-spec(rchr(string(), char()) -> integer()).
rchr(String, CharToFind) -> string:rchr(String, CharToFind).

-spec(str(string(), string()) -> integer()).
str(String, SubstringToFind) -> string:str(String, SubstringToFind).

-spec(rstr(string(), string()) -> integer()).
rstr(String, SubstringToFind) -> string:rstr(String, SubstringToFind).

-spec(substr(string(), integer()) -> string()).
substr(String, Start) -> string:substr(String, Start).

-spec(substr(string(), integer(), integer()) -> string()).
substr(String, Start, Length) -> string:substr(String, Start, Length).

-spec(sub_string(string(), integer()) -> string()).
sub_string(String, Start) -> string:sub_string(String, Start).

-spec(sub_string(string(), integer(), integer()) -> string()).
sub_string(String, Start, End) -> string:sub_string(String, Start, End).

-spec(span(string(), string()) -> string()).
span(String, Tail) -> string:span(String, Tail).

-spec(cspan(string(), string()) -> string()).
cspan(String, CharsList) -> string:cspan(String, CharsList).

% in tokens, note that "list() of Chars" meaning of string accurate for SeparatorChars.

-spec(tokens(string(), string()) -> list()).
tokens(String, SeparatorChars) -> string:tokens(String, SeparatorChars).

-spec(join(list(), string()) -> string()).
join(ListOfStrings, Separator) -> string:join(ListOfStrings, Separator).

-spec(words(string()) -> integer()).
words(String) -> string:words(String).

-spec(words(string(), char()) -> integer()).
words(String, WordSeparator) -> string:words(String, WordSeparator).

-spec(chars(char(), integer()) -> string()).
chars(Character, Number) -> string:chars(Character, Number).

-spec(chars(char(), integer(), string()) -> string()).
chars(Character, Number, Tail) -> string:chars(Character, Number, Tail).

-spec(copies(string(), integer()) -> string()).
copies(String, RepeatCount) -> string:copies(String, RepeatCount).

-spec(strip(string()) -> string()).
strip(String) -> string:strip(String).

% note left | right | both
-spec(strip(string(), atom()) -> string()).
strip(String, Direction) -> string:strip(String, Direction).

-spec(strip(string(), atom(), char()) -> string()).
strip(String, Direction, Character) -> string:strip(String, Direction, Character).

-spec(left(string(), integer()) -> string()).
left(String, TargetLength) -> string:left(String, TargetLength).

-spec(left(string(), integer(), char()) -> string()).
left(String, TargetLength, PaddingCharacter) -> string:left(String, TargetLength, PaddingCharacter).

-spec(right(string(), integer()) -> string()).
right(String, TargetLength) -> string:right(String, TargetLength).

-spec(right(string(), integer(), char()) -> string()).
right(String, TargetLength, PaddingCharacter) -> string:right(String, TargetLength, PaddingCharacter).

-spec(centre(string(), integer()) -> string()).
centre(String, TargetLength) -> string:left(String, TargetLength).

-spec(centre(string(), integer(), char()) -> string()).
centre(String, TargetLength, PaddingCharacter) -> string:centre(String, TargetLength, PaddingCharacter).

-spec(reverse(string()) -> string()).
reverse(String) -> lists:reverse(String).

-spec(reverse(string(), string()) -> string()).
reverse(String, Tail) -> lists:reverse(String, Tail).

-spec(to_float(string()) -> float()).
to_float(String) -> string:to_float(String).

-spec(to_integer(string()) -> integer()).
to_integer(String) -> string:to_integer(String).

-spec(to_lower(string()) -> string()).
to_lower(String) -> string:to_lower(String).

-spec(to_upper(string()) -> string()).
to_upper(String) -> string:to_upper(String).

-spec(integer_to_list(integer()) -> string()).
integer_to_list(Integer) -> erlang:integer_to_list(Integer).

-spec(integer_to_list(integer(), integer()) -> string()).
integer_to_list(Integer, Base) -> erlang:integer_to_list(Integer, Base).

-spec(float_to_list(float()) -> string()).
float_to_list(Float) -> erlang:float_to_list(Float).

-spec(fun_to_list(fun()) -> string()).
fun_to_list(Fun) -> erlang:fun_to_list(Fun).

-spec(list_to_atom(string()) -> atom()).
list_to_atom(String) -> erlang:list_to_atom(String).