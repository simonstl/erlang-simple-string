%% @author Simon St.Laurent <simonstl@simonstl.com>
%% @doc Wrapper library for functions applied to strings

%%% (2-clause) Simplified BSD license

%%% Copyright (c) 2013, Simon St.Laurent
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without 
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.

%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
%%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
%%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

-module(sstr).
-export([len/1, length/1, concat/1, concat/2, append/1, append/2, nth/2, hd/1]).
-export([chr/2, str/2, substr/2, substr/3, sub_string/2, sub_string/3, tokens/2]).
-export([join/2, words/1, words/2, chars/2, chars/3, copies/2, strip/1, strip/2]).
-export([strip/3, left/2, left/3, right/2, right/3, centre/2, centre/3, reverse/1]).
-export([reverse/2, to_float/1, to_integer/1, to_lower/1, to_upper/1]).
-export([integer_to_list/1, integer_to_list/2, float_to_list/1, fun_to_list/1]). 
-export([list_to_atom/1, equal/2, rchr/2, rstr/2, span/2, cspan/2]).

%% @doc Returns the number of characters in a string
%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(len(string()) -> integer()).
len(String) -> string:len(String).

%% @doc Returns the number of characters in a string
%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(length(string()) -> integer()).
length(String) -> erlang:length(String).

%% @doc reports whether two strings are equal.
%% @todo (performs no Unicode normalization, just checks that all characters are same?)

-spec(equal(string(), string()) -> boolean()).
equal(String1, String2) -> string:equal(String1, String2).


%% @doc Returns a single string containing all the parts from the ListOfStrings concatenated in the order they appear in the list.

-spec(concat(list()) -> string()).
concat(ListOfStrings) -> lists:concat(ListOfStrings).


%% @doc Returns a single string containing the two parts from the arguments.

-spec(concat(string(), string()) -> string()).
concat(String1, String2) -> string:concat(String1, String2).


%% @doc Returns a single string containing all the parts from the arguments in the ListOfStrings.

-spec(append(list()) -> string()).
append(ListOfStrings) -> lists:append(ListOfStrings).


%% @doc Returns a single string containing all the parts from the arguments.

-spec(append(string(), string()) -> string()).
append(String1, String2) -> lists:append(String1, String2).


%% @doc Returns the character at the specified position.
%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(nth(integer(), string()) -> char()).
nth(Position, String) -> lists:nth(Position, String).


%% @doc Returns the first character of the string.

-spec(hd(string()) -> char()).
hd(String) -> erlang:hd(String).



%% @doc Returns the position where the specified character first appears

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(chr(string(), char()) -> integer()).
chr(String, CharToFind) -> string:chr(String, CharToFind).


%% @doc Returns the position where the specified character last appears.

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(rchr(string(), char()) -> integer()).
rchr(String, CharToFind) -> string:rchr(String, CharToFind).


%% @doc Returns the position of the first appearance of a substring in a string.

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(str(string(), string()) -> integer()).
str(String, SubstringToFind) -> string:str(String, SubstringToFind).


%% @doc Returns the position of the last appearance of a substring in a string.

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(rstr(string(), string()) -> integer()).
rstr(String, SubstringToFind) -> string:rstr(String, SubstringToFind).


%% @doc Returns a segment of a string from a given position to the end.

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(substr(string(), integer()) -> string()).
substr(String, Start) -> string:substr(String, Start).


%% @doc Returns a segment of a string from a given position of a given length.

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(substr(string(), integer(), integer()) -> string()).
substr(String, Start, Length) -> string:substr(String, Start, Length).


%% @doc Returns a segment from a string between a Start position and the end.

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(sub_string(string(), integer()) -> string()).
sub_string(String, Start) -> string:sub_string(String, Start).


%% @doc Returns segment from a string between two positions.

%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(sub_string(string(), integer(), integer()) -> string()).
sub_string(String, Start, End) -> string:sub_string(String, Start, End).


%% @doc Returns the length of the initial segment of a string that contains only the characters in CharsList.
%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(span(string(), string()) -> integer()).
span(String,  CharsList) -> string:span(String,  CharsList).


%% @doc Returns the length of the initial segment of a string that contains only characters NOT in CharsList.
%% @todo (need to consider what "characters" means in this context - un-normalized Unicode?)

-spec(cspan(string(), string()) -> integer()).
cspan(String, CharsList) -> string:cspan(String, CharsList).


%% @doc Returns a list of tokens extracted from the string, using the list of characters in SeparatorChars to decompose the string.

% in tokens, note that "list() of Chars" meaning of string accurate for SeparatorChars.

-spec(tokens(string(), string()) -> list()).
tokens(String, SeparatorChars) -> string:tokens(String, SeparatorChars).


%% @doc Returns  a string made from the list of pieces with specified separators added.

-spec(join(list(), string()) -> string()).
join(ListOfStrings, Separator) -> string:join(ListOfStrings, Separator).



%% @doc Returns the number of words in the string, where blanks are used to indicate breaks between words.
%% @todo figure out if "blanks" is just "spaces" or is broader.

-spec(words(string()) -> integer()).
words(String) -> string:words(String).


%% @doc Returns the number of words in the string, where WordSeparator is used to indicate breaks between words.

-spec(words(string(), char()) -> integer()).
words(String, WordSeparator) -> string:words(String, WordSeparator).



%% @doc Returns a string that repeats the given character RepeatCount number of times.

-spec(chars(char(), integer()) -> string()).
chars(Character, Number) -> string:chars(Character, Number).


%% @doc Returns a string that repeats the given character RepeatCount number of times and appends a Tail.

-spec(chars(char(), integer(), string()) -> string()).
chars(Character, Number, Tail) -> string:chars(Character, Number, Tail).



%% @doc Returns a string that repeats the given string RepeatCount number of times.

-spec(copies(string(), integer()) -> string()).
copies(String, RepeatCount) -> string:copies(String, RepeatCount).




%% @doc Returns a string with both leading and trailing whitespace removed

-spec(strip(string()) -> string()).
strip(String) -> string:strip(String).



%% @doc Returns a string with leading and/or trailing whitespace removed.

% note left | right | both
-spec(strip(string(), atom()) -> string()).
strip(String, Direction) -> string:strip(String, Direction).



%% @doc Returns a string with leading and/or trailing specified characters removed.

-spec(strip(string(), atom(), char()) -> string()).
strip(String, Direction, Character) -> string:strip(String, Direction, Character).




%% @doc Returns a string of a specified length, padded with spaces on the left if needed.
%% @todo (need to consider what "characters" for TargetLength means in this context - un-normalized Unicode?)



-spec(left(string(), integer()) -> string()).
left(String, TargetLength) -> string:left(String, TargetLength).



%% @doc Returns a string of a specified length, padded with PaddingCharacter on the  left if needed.
%% @todo (need to consider what "characters" for TargetLength means in this context - un-normalized Unicode?)


-spec(left(string(), integer(), char()) -> string()).
left(String, TargetLength, PaddingCharacter) -> string:left(String, TargetLength, PaddingCharacter).

%% @doc Returns a string of a specified length, padded with spaces on the right if needed.
%% @todo (need to consider what "characters" for TargetLength means in this context - un-normalized Unicode?)

-spec(right(string(), integer()) -> string()).
right(String, TargetLength) -> string:right(String, TargetLength).


%% @doc Returns a string of a specified length, padded with PaddingCharacter on the  right if needed.
%% @todo (need to consider what "characters" for TargetLength means in this context - un-normalized Unicode?)

-spec(right(string(), integer(), char()) -> string()).
right(String, TargetLength, PaddingCharacter) -> string:right(String, TargetLength, PaddingCharacter).



%% @doc Returns a string of a specified length, padded with spaces on the left and right if needed.
%% @todo (need to consider what "characters" for TargetLength means in this context - un-normalized Unicode?)

-spec(centre(string(), integer()) -> string()).
centre(String, TargetLength) -> string:left(String, TargetLength).


%% @doc Returns a string of a specified length, padded with PaddingCharacter on the left and right if needed.
%% @todo (need to consider what "characters" for TargetLength means in this context - un-normalized Unicode?)

-spec(centre(string(), integer(), char()) -> string()).
centre(String, TargetLength, PaddingCharacter) -> string:centre(String, TargetLength, PaddingCharacter).



%% @doc Returns a string which is in reverse order.

-spec(reverse(string()) -> string()).
reverse(String) -> lists:reverse(String).


%% @doc Returns a string which is in reverse order, with a tail added to the end.

-spec(reverse(string(), string()) -> string()).
reverse(String, Tail) -> lists:reverse(String, Tail).



%% @doc Converts a string to a float representing its contents.

-spec(to_float(string()) -> float()).
to_float(String) -> string:to_float(String).

%% @doc Converts a string to an integer representing its contents.

-spec(to_integer(string()) -> integer()).
to_integer(String) -> string:to_integer(String).

%% @doc Returns an lower-case version of a string.

%% @todo need to figure out how Unicode may interact here.

-spec(to_lower(string()) -> string()).
to_lower(String) -> string:to_lower(String).

%% @doc Returns an upper-case version of a string.

%% @todo need to figure out how Unicode may interact here.

-spec(to_upper(string()) -> string()).
to_upper(String) -> string:to_upper(String).

%% @doc Converts an integer to a string using Base 10 representation.

-spec(integer_to_list(integer()) -> string()).
integer_to_list(Integer) -> erlang:integer_to_list(Integer).

%% @doc Converts an integer to a string representing it in the specified Base.

-spec(integer_to_list(integer(), integer()) -> string()).
integer_to_list(Integer, Base) -> erlang:integer_to_list(Integer, Base).

%% @doc Converts a float to a string

-spec(float_to_list(float()) -> string()).
float_to_list(Float) -> erlang:float_to_list(Float).

%% @doc Converts a fun to a string

-spec(fun_to_list(fun()) -> string()).
fun_to_list(Fun) -> erlang:fun_to_list(Fun).

%% @doc Converts a string to an atom

-spec(list_to_atom(string()) -> atom()).
list_to_atom(String) -> erlang:list_to_atom(String).


% need to eventually consider http://www.w3.org/TR/charmod-norm/#sec-FullyNormalized