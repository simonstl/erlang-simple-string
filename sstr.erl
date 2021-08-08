%%%
%%% CopyrightBegin%
%%%
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
%%%
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
%%% 
%%% %CopyrightEnd%
%%% 
%%% Wrapper library for Erlang functions applied to strings
%%%
%% @author Simon St.Laurent <simonstl@simonstl.com> [http://simonstl.com]
%% @copyright 2012 by Simon St.Laurent
%% @version 0.0001
%% @doc A single library collecting string functions from elsewhere in Erlang.
%% The <code>sstr</code> module assembles string-related functions from the following libraries:
%%
%% <ul>
%% <li><code>string</code> of course.</li>
%% 
%% <li><code>unicode</code> because <abbr title="Open Telecom Platform">OTP 20</abbr> provides such functionality.</li>
%%
%% <li><code>lists</code>, because (typical) Erlang strings are lists.</li>
%%
%% <li><code>erlang</code> because some of the basics lurk there.</li>
%%
%% <li><code>re</code> because regular expressions are a speedy
%% and effective way to manipulate string content.</li>
%% </ul>
%%
%% Developers working with normalized text will probably find most of these functions straightforward.  
%% However, a few basic techniques for inspecting strings may be useful when Unicode's variations create 
%% surprises.
%%
%% For example, the word &#xE9;tudes can be represented two ways.  The first version represents
%% the opening &#xE9; as a single character:
%% ```
%% 1> Test="&#xE9;tude".
%% "&#xE9;tude"
%% '''
%% You can see what's inside of that string with <code>io:format</code>'s <code>~w</code>
%% control sequence:
%% ```
%% 2> io:format("~w~n",[Test]).
%%[233,116,117,100,101]
%% ok
%% '''
%% That's five characters, starting with character 233 (or E9 in hex).
%%
%% The second version represents the &#xE9; as a regular e plus an acute accent.  (No, this isn't
%% a smart approach for that easily-managed character, but you may encounter this approach
%% used for other characters or in cases where someone decomposed all the Unicode.)
%% ```
%% 3> Test2=[101,769,116,117,100,101].
%% [101,769,116,117,100,101]
%% 4> io:format("~ts~n",[Test2]).
%% &#xE9;tude
%% ok
%% '''
%% In this case, the &#xE9; is represented by two characters, 101 and 769.  That means 
%% the list underneath the string is six characters long.
%%
%% These variations may lead to surprises. [Need to point to normalization / denormalization tools.]
%% ```
%% 5> io:format("~ts~n",[unicode:characters_to_list(Test)]).
%% &#xE9;tude
%% ok
%% '''
%%

-module(sstr).
-export([len/1, length/1, concat/1, concat/2, append/1, append/2, nth/2, hd/1]).
-export([chr/2, str/2, substr/2, substr/3, sub_string/2, sub_string/3, tokens/2]).
-export([join/2, words/1, words/2, chars/2, chars/3, copies/2, strip/1, strip/2]).
-export([strip/3, left/2, left/3, right/2, right/3, centre/2, centre/3, reverse/1]).
-export([reverse/2, to_float/1, to_integer/1, to_lower/1, to_upper/1]).
-export([integer_to_list/1, integer_to_list/2, float_to_list/1, fun_to_list/1]). 
-export([list_to_atom/1, equal/2, rchr/2, rstr/2, span/2, cspan/2]).
-export([run/4, replace/5, split/4]).

-type chardata() :: unicode:chardata().
-type direction() :: string:direction().
-type grapheme_cluster() :: string:grapheme_cluster().

-type nl_spec() :: cr | crlf | lf | anycrlf | any.
-type compile_option() :: 
    unicode | anchored | caseless | dollar_endonly | dotall |
    extended | firstline | multiline | no_auto_capture |
    dupnames | ungreedy | 
    {newline, nl_spec()} |
    bsr_anycrlf | bsr_unicode | no_start_optimize | ucp |
    never_utf.


%% @doc Returns the length of as string.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#len-1 string:len/1].
%% 
%% @deprecated Please use the {@link length/1} instead.
%% @param String input string
%% @returns length of a data list.

-spec len(String) -> Length when
      String:: chardata(),
      Length :: non_neg_integer().
len(String) -> string:len(String).

%% @doc Counts the number of characters in a string.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#length-1 string:length/1].
%% 
%% @param String input string
%% @returns count of characters in a string.

-spec length(String) -> Length when
      String:: chardata(),
      Length :: non_neg_integer().
length(String) -> string:length(String).

%% @doc Checks whether two strings are equal. This functions is case sensative.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#equal-4 string:equal/4].
%% 
%% @param String1 input string
%% @param String2 input string
%% @returns Result of comparison unicode normalized character lists.

-spec equal(String1, String2) -> Result when
      String1 :: chardata(),
      String2 :: chardata(),
      Result :: boolean().
equal(String1, String2) -> string:equal(String1, String2, false, nfc).

%% @doc Returs a single string containing all the parts from the ListOfStrings concatenated in the order they appear in the list.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/lists.html#concat-1 lists:concat/1].
%% 
%% @param ListOfStrings list of strings
%% @returns new string

-spec concat(ListOfStrings) -> NewString when 
      ListOfStrings :: [chardata()],
      NewString :: chardata().
concat(ListOfStrings) -> lists:concat(ListOfStrings).


%% @doc Returs a single string containing the two parts from the arguments.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/unicode.html#characters_to_list-2 unicode:characters_to_list/2].
%% 
%% @param String1 input string
%% @param String2 input string
%% @returns new string

-spec concat(String1, String2) -> NewString when 
      String1 :: chardata(), 
      String2 :: chardata(),
      NewString :: chardata().
concat(String1, String2) -> unicode:characters_to_list([String1, String2], utf8 ).


%% @doc Returs a single string containing all the parts from the arguments in the ListOfStrings.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/lists.html#append-1 lists:append/1].
%% 
%% @param List list of strings
%% @returns new string

-spec append(ListOfStrings) -> NewString when
      ListOfStrings :: [chardata()],
      NewString :: chardata().
append(ListOfStrings) -> lists:append(ListOfStrings).


%% @doc Returs a single string containing all the parts from the arguments.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:append("abc", "def").
%% "abcdef"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/lists.html#append-1 lists:append/1].
%% 
%% @param String1 input string
%% @param String2 input string
%% @returns new string

-spec append(String1, String2) -> NewString when 
      String1 :: chardata(),
      String2 :: chardata(),
      NewString :: chardata().
append(String1, String2) -> lists:append(String1, String2).


%% @doc Returns the character at the specified position.
%% Position start at 0.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#length-1 string:length/1], 
%% [http://erlang.org/doc/man/string.html#next_grapheme-1 string:next_grapheme/1].
%% 
%% @param Position the specified position
%% @param String input string
%% @returns character as a string

-spec nth(Position, String) -> Character when 
      Position :: non_neg_integer(),
      String :: chardata(),
      Character :: chardata() | {error, chardata()} | {error, badarg}.
nth(Position, String) -> 
    Length = string:length(String),
    nth(Position, String, Length, 0).

-spec nth(Position, String, Length, Start) -> 
    chardata() | {error, chardata()} | {error, badarg} when
      Position :: non_neg_integer(),
      String :: chardata(),
      Length :: non_neg_integer(), 
      Start :: non_neg_integer().
nth(Position, _String, Length, Start) when Length =< Position; Start == Length ->
    {error, badarg};
nth(Position, String, Length, Start)  ->
    [Grapheme|Rest] = string:next_grapheme(String),
    nth(Position, Rest, Length, Start, Grapheme).

nth(Position, _String, _Length, Start, Grapheme) when Position == Start ->
  unicode:characters_to_list([Grapheme]);
nth(Position, String, Length, Start, _Grapheme) ->
  nth(Position,String, Length, Start+1).


%% @doc Returns the first character of the string.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#next_grapheme-1 string:next_grapheme/1], 
%% [http://erlang.org/doc/man/unicode.html#characters_to_list-1 unicode:characters_to_list/1].
%% 
%% @param String input string
%% @returns character as a string

-spec hd(String) -> FirstCharacter when
      String :: chardata(),
      FirstCharacter :: chardata().
hd(String) -> 
    [Grapheme|_] = string:next_grapheme(String),
    unicode:characters_to_list([Grapheme]).


%% @doc Returns the position where the specified character first appears
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#find-2 string:find/2].
%% 
%% @param String input string
%% @param CharToFind char to find as a string
%% @returns position
%% @see sstr:length/1

-spec chr(String, CharToFind) -> Position when
      String :: chardata(),
      CharToFind :: chardata(),
      Position :: non_neg_integer() | {error, badarg}.
chr(String, CharToFind) -> 
    Result = string:find(String,CharToFind),
    Length = sstr:length(String),
    chr(Length, String, Result).

chr(_Length, _String, Result) when Result == nomatch -> {error, badarg};
chr(Length, _String, Result)-> Length - sstr:length(Result).


%% @doc Returns the position where the specified character last appears.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#find-2 string:find/2].
%% 
%% @param String input string
%% @param CharToFind char to find as a string
%% @returns position
%% @see sstr:length/1
%% @see sstr:chr/3

-spec rchr(String, CharToFind) -> Position when
      String :: chardata(),
      CharToFind :: chardata(),
      Position :: non_neg_integer() | {error, badarg}.
rchr(String, CharToFind) -> 
    Result = string:find(String,CharToFind,	trailing),
    Length = sstr:length(String),
    chr(Length, String, Result).  


%% @doc Returns the position of the first appearance of a substring in a string.
%% @param String input string
%% @param SubstringToFind substring
%% @returns position
%% @see sstr:chr/2

-spec str(String, SubstringToFind) -> Position when
      String :: chardata(),
      SubstringToFind :: chardata(),
      Position :: non_neg_integer() | {error, badarg}.
str(String, SubstringToFind) -> chr(String, SubstringToFind).

%% @doc Returns the position of the last appearance of a substring in a string.
%% @param String input string
%% @param SubstringToFind substring
%% @returns position
%% @see sstr:rchr/2

-spec rstr(String, SubstringToFind) -> Position when
      String :: chardata(),
      SubstringToFind :: chardata(),
      Position :: non_neg_integer() | {error, badarg}.
rstr(String, SubstringToFind) -> rchr(String, SubstringToFind).


%% @doc Returns a segment of a string from a given position to the end.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#slice-2 string:slice/2]. 
%%
%% @param String input string
%% @param Start start position
%% @returns sstr:substring

-spec substr(String, Start) -> SubString when
      String :: chardata(), 
      Start :: non_neg_integer(),
      SubString :: chardata().
substr(String, Start) -> string:slice(String, Start).


%% @doc Returns a segment of a string from a given position of a given length.
%% <br/>
%% <b>See also:</b> [http://erlang.org/doc/man/string.html#slice-4 string:slice/4].
%% 
%% @param String input string
%% @param Start start position
%% @param Length a length of the segment
%% @returns sstr:substring

-spec substr(String, Start, Length) -> SubString when
      String :: chardata(), 
      Start :: non_neg_integer(),
      Length :: non_neg_integer(),
      SubString :: chardata().
substr(String, Start, Length) -> string:slice(String, Start, Length).


%% @doc Returns a segment from a string between a Start position and the end.
%% @param String input string
%% @param Start start position
%% @returns substring
%% @see sstr:substr/2

-spec sub_string(String, Start) -> SubString when
      String :: chardata(), 
      Start :: non_neg_integer(),
      SubString :: chardata().
sub_string(String, Start) -> substr(String, Start).


%% @doc Returns segment from a string between two positions.
%% @param String input string
%% @param Start start position
%% @param End end position
%% @returns substring
%% @see sstr:substr/3

-spec sub_string(String, Start, End) -> SubString when
      String :: chardata(), 
      Start :: non_neg_integer(),
      End :: non_neg_integer(),
      SubString :: chardata().
sub_string(String, Start, End) -> substr(String, Start, End-Start).


%% @doc Returns the length of the initial segment of a string that contains only the characters in CharsList.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#take-2 string:take/2].
%% 
%% @param String input string
%% @param CharsList list of characters
%% @returns length of the segment
%% @see length/1

-spec span(String, CharsList) -> Length when 
      String :: chardata(),
      CharsList :: [grapheme_cluster()],
      Length :: non_neg_integer().
span(String,  CharsList) -> 
    {Result,_} = string:take(String,  CharsList),
    sstr:length(Result).


%% @doc Returns the length of the initial segment of a string that contains only characters NOT in CharsList.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:cspan("\t    abcdef", " \t").
%% 0
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#take-2 string:take/3].
%% 
%% @param String input string
%% @param CharsList list of characters
%% @returns length of the segment
%% @see sstr:length/1

-spec cspan(String, CharsList) -> Length when 
      String :: chardata(),
      CharsList :: [grapheme_cluster()],
      Length :: non_neg_integer().
cspan(String, CharsList) -> 
    {Result,_} = string:take(String, CharsList, true),
    sstr:length(Result).


%% @doc Returns a list of tokens extracted from the string, using the list of characters in SeparatorChars to decompose the string.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:tokens("abc defxxghix jkl", "x ").
%% ["abc", "def", "ghi", "jkl"]
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#lexemes-2 string:lexemes/2].
%% 
%% @param String input string
%% @param SeparatorChars string separator
%% @returns list of tokens

-spec tokens(String, SeparatorChars) -> List when
      String :: chardata(),
      SeparatorChars :: [grapheme_cluster()],
      List :: [chardata()].
tokens(String, SeparatorChars) -> string:lexemes(String, SeparatorChars).


%% @doc Returns a string made from the list of pieces with specified separators added.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:join(["one", "two", "three"], ", ").
%% "one, two, three"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/lists.html#join-2 lists:join/2].
%% @param List the list of pieces
%% @param Separator separator of pieces
%% @returns new string
%% @see sstr:concat/1

-spec join(List, Separator) -> String when
      List :: [chardata()],
      Separator :: chardata(),
      String :: chardata().
join(ListOfStrings, Separator) -> 
    List = lists:join(Separator, ListOfStrings),
    sstr:concat(List).


%% @doc Returns the number of words in the string, where blanks are used to indicate breaks between words.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:words("Hello old boy!").
%% 3
%% '''
%% @param String input string
%% @returns the number of words in the string
%% @see words/2

-spec words(String) -> Count when
      String :: chardata(),
      Count :: non_neg_integer().
words(String) -> sstr:words(String, " " ++ [$\r,$\n]).


%% @doc Returns the number of words in the string, where WordSeparator is used to indicate breaks between words.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:words(" Hello old boy!", [$o]).
%% 4
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#lexemes-2 string:lexemes/2],
%% [http://erlang.org/doc/man/erlang.html#length-1 erlang:length/1].
%% 
%% @param String input string
%% @param WordSeparator separator
%% @returns the number of words in the string

-spec words(String, WordSeparator) -> Count when
      String :: chardata(),
      WordSeparator :: grapheme_cluster(),
      Count :: non_neg_integer().
words(String, WordSeparator) -> 
    List = string:lexemes(String, WordSeparator),
    erlang:length(List).


%% @doc Returns a string that repeats the given character RepeatCount number of times.
%% 
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/lists.html#duplicate-2 lists:duplicate/2]. 
%%
%% @param Character a character
%% @param RepeatCount the number of times
%% @returns new string
%% @see concat/1	

-spec chars(Character, RepeatCount) -> String when
      Character :: grapheme_cluster(),
      RepeatCount :: non_neg_integer(),
      String :: chardata().
chars(Character, RepeatCount) -> 
    List = lists:duplicate(RepeatCount, Character),
    sstr:concat(List).


%% @doc Returns a string that repeats the given character RepeatCount number of times and appends a Tail.
%% 
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/lists.html#duplicate-2 lists:duplicate/2].
%% 
%% @param Character a character
%% @param RepeatCount the number of times
%% @param Tail additional string
%% @returns new string
%% @see concat/2

-spec chars(Character, RepeatCount, Tail) -> String when
      Character :: grapheme_cluster(),
      RepeatCount :: non_neg_integer(),
      Tail :: chardata(),
      String :: chardata().
chars(Character, RepeatCount, Tail) -> 
    String = sstr:chars(Character, RepeatCount),
    sstr:concat(String, Tail).
    

%% @doc Returns a string that repeats the given string RepeatCount number of times.
%% @equiv chars/2
%% @param String input string
%% @param RepeatCount the number of times
%% @returns new string
%% @see sstr:chars/2

-spec copies(String, RepeatCount) -> NewString when
      String :: chardata(),
      RepeatCount :: non_neg_integer(),
      NewString :: chardata().
copies(String, RepeatCount) -> sstr:chars(String, RepeatCount).


%% @doc Returns a string with both leading and trailing whitespace removed
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> string:strip("\r   Hello     \n").
%% "Hello"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#trim-1 string:trim/1].
%%
%% @param String input string
%% @returns new string

-spec strip(String) -> NewString when
      String :: chardata(),
      NewString :: chardata().
strip(String) -> string:trim(String).


%% @doc Returns a string with leading and/or trailing whitespace removed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:strip("Hello     \n", leading).
%% "Hello     \n"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#trim-2 string:trim/2].
%%
%% @param String input string
%% @param Direction leading | trailing | both
%% @returns new string

-spec strip(String, Direction) -> NewString when
      String :: chardata(),
      Direction :: direction() | both,
      NewString :: chardata().
strip(String, Direction) -> string:trim(String, Direction).


%% @doc Returns a string with leading and/or trailing specified characters removed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:strip(<<".Hello.\n">>, trailing, "\n.").
%% <<".Hello">>
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#trim-3 string:trim/3].
%%
%% @param String input string
%% @param Direction leading | trailing | both
%% @returns new string

-spec strip(String, Direction, Characters) -> NewString when
      String :: chardata(),
      Direction :: direction() | both,
      Characters :: [string:grapheme_cluster()], 
      NewString :: chardata().
strip(String, Direction, Characters) -> string:trim(String, Direction, Characters).


%% @doc Returns a string of a specified length, padded with spaces on the left if needed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:left("Hello", 10).
%% "Hello     "
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#pad-4 string:pad/4], 
%% [http://erlang.org/doc/man/lists.html#flatten-1 lists:flatten/1].
%% 
%% @param String input string
%% @param TargetLength length of a new string
%% @returns new string

-spec left(String, TargetLength) -> NewString when
      String :: chardata(),
      TargetLength :: non_neg_integer(),
      NewString :: chardata().
left(String, TargetLength) -> 
    List = string:pad(String, TargetLength, leading, " "),
    lists:flatten(List).
    

%% @doc Returns a string of a specified length, padded with PaddingCharacter on the  left if needed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:left("Hello", 10, $.).
%% ".....Hello"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#pad-4 string:pad/4], 
%% [http://erlang.org/doc/man/lists.html#flatten-1 lists:flatten/1].
%% 
%% @param String input string
%% @param TargetLength length of a new string
%% @returns new string

-spec left(String, TargetLength, PaddingCharacter) -> NewString when
      String :: chardata(),
      TargetLength :: non_neg_integer(),
      PaddingCharacter :: string:grapheme_cluster(),
      NewString :: chardata().
left(String, TargetLength, PaddingCharacter) -> 
    List = string:pad(String, TargetLength, leading, PaddingCharacter),
    lists:flatten(List).


%% @doc Returns a string of a specified length, padded with spaces on the right if needed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:right("Hello", 10).
%% "Hello     "
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#pad-4 string:pad/4], 
%% [http://erlang.org/doc/man/lists.html#flatten-1 lists:flatten/1]. 
%%
%% @param String input string
%% @param TargetLength length of a new string
%% @returns new string

-spec right(String, TargetLength) -> NewString when
      String :: chardata(),
      TargetLength :: non_neg_integer(),
      NewString :: chardata().
right(String, TargetLength) -> 
    List = string:pad(String, TargetLength, trailing, " "),
    lists:flatten(List).


%% @doc Returns a string of a specified length, padded with PaddingCharacter on the  right if needed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:left("Hello", 10, $.).
%% "Hello....."
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#pad-4 string:pad/4], 
%% [http://erlang.org/doc/man/lists.html#flatten-1 lists:flatten/1]. 
%%
%% @param String input string
%% @param TargetLength length of a new string
%% @returns new string

-spec right(String, TargetLength, PaddingCharacter) -> NewString when
      String :: chardata(),
      TargetLength :: non_neg_integer(),
      PaddingCharacter :: string:grapheme_cluster(),
      NewString :: chardata().
right(String, TargetLength, PaddingCharacter) -> 
    List = string:pad(String, TargetLength, trailing, PaddingCharacter),
    lists:flatten(List).


%% @doc Returns a string of a specified length, padded with spaces on the left and right if needed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:centre("Hello", 10).
%% "     Hello     "
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#pad-4 string:pad/4], 
%% [http://erlang.org/doc/man/lists.html#flatten-1 lists:flatten/1].
%%
%% @param String input string
%% @param TargetLength length of a new string
%% @returns new string

-spec centre(String, TargetLength) -> NewString when
      String :: chardata(),
      TargetLength :: non_neg_integer(),
      NewString :: chardata().
centre(String, TargetLength) -> 
    List = string:pad(String, TargetLength, both, " "),
    lists:flatten(List).


%% @doc Returns a string of a specified length, padded with PaddingCharacter on the left and right if needed.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:centre("Hello", 10, $.).
%% ".....Hello....."
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#pad-4 string:pad/4],  
%% [http://erlang.org/doc/man/lists.html#flatten-1 lists:flatten/1]. 
%% @param String input string
%% @param TargetLength length of a new string
%% @returns new string

-spec centre(String, TargetLength, PaddingCharacter) -> NewString when
      String :: chardata(),
      TargetLength :: non_neg_integer(),
      PaddingCharacter :: string:grapheme_cluster(),
      NewString :: chardata().
centre(String, TargetLength, PaddingCharacter) -> 
    List = string:pad(String, TargetLength, both, PaddingCharacter),
    lists:flatten(List).


%% @doc Returns a string which is in reverse order.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> Reverse = sstr:reverse("ÅÄÖ").
%% [79,776,65,776,65,778]
%% 2> io:format("~ts~n",[Reverse]).
%% ÖÄÅ
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#reverse-1 string:reverse/1], 
%% [http://erlang.org/doc/man/lists.html#flatten-1 lists:flatten/1]. 
%%
%% @param String input string
%% @returns reversed input string

-spec reverse(String) -> NewString when
      String :: chardata(),
      NewString :: chardata().
reverse(String) -> 
    ReversedString = string:reverse(unicode:characters_to_nfd_binary(String)),
    lists:flatten(ReversedString).


%% @doc Returns a string which is in reverse order, with a tail added to the end.
%% @param String input string
%% @param Tail additional string
%% @returns new string
% @see sstr:reverse/1
% @see sstr:concat/2

-spec reverse(String, Tail) -> NewString when
      String :: chardata(),
      Tail :: chardata(),
      NewString :: chardata().
reverse(String, Tail) -> 
    ReversedString = sstr:reverse(String),
    sstr:concat(ReversedString, Tail).


%% @doc Converts a string to a float representing its contents.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> F1 = sstr:to_float("1.0-1.0e-1"),
%% 1> F2 = sstr:to_float("-1.0e-1"),
%% 1> F1+F2.
%% 0.9
%% 2> sstr:to_float("3/2=1.5").
%% {error,no_float}
%% 3> sstr:to_float("-1.5eX").
%% -1.5
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#to_float-1 string:to_float/1]. 
%%
%% @param String input string
%% @returns a float value

-spec to_float(String) -> Float | {error, Reason} when 
      String :: chardata(),
      Float :: float(),
      Reason :: no_float | badarg.
to_float(String) -> 
    case string:to_float(String) of
        {error, Reason} -> {error, Reason};
        {Float, _Rest} -> Float
    end.


%% @doc Converts a string to an integer representing its contents.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> I1 = sstr:to_integer("33"),
%% 1> I2 = sstr:to_integer("+22"),
%% 1> I1-I2.
%% 11
%% 2> sstr:to_integer("0.5").
%% 0
%% 3> sstr:to_integer("x=2").
%% {error,no_integer}
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#to_integer-1 string:to_integer/1]. 
%%
%% @param String input string
%% @returns an integer value

-spec to_integer(String) -> Int | {error, Reason} when
      String :: chardata(),
      Int :: integer(),
      Reason :: no_integer | badarg.
to_integer(String) -> 
    case string:to_integer(String) of
        {error, Reason} -> {error, Reason};
        {Int, _Rest} -> Int
    end.

%% @doc Returns an lower-case version of a string.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:to_lower("MICHAŁ").
%% "michał"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#lowercase-1 string:lowercase/1]. 
%%
%% @param String input string
%% @returns lower case string

-spec to_lower(String) -> NewString when
      String :: chardata(),
      NewString :: chardata() | {error, chardata()}.
to_lower(String) -> string:lowercase(String).

%% @doc Returns an upper-case version of a string.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:to_upper("michał").
%% "MICHAŁ"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/string.html#uppercase-1 string:uppercase/1]. 
%%
%% @param String input string
%% @returns upper case string

-spec to_upper(String) -> NewString when
      String :: chardata(),
      NewString :: chardata() | {error, chardata()}.
to_upper(String) -> string:uppercase(String).

%% @doc Converts an integer to a string using Base 10 representation.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:integer_to_list(77).
%% "77"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/erlang.html#integer_to_list-1 erlang:integer_to_list/1]. 
%%
%% @param Int integer value
%% @returns string

-spec integer_to_list(Int) -> String when
      Int :: integer(),
      String :: string().
integer_to_list(Integer) -> erlang:integer_to_list(Integer).

%% @doc Converts an integer to a string representing it in the specified Base.
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:integer_to_list(36,36).
%% "10"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/erlang.html#integer_to_list-2 erlang:integer_to_list/2]. 
%%
%% @param Int integer value
%% @returns string

-spec integer_to_list(Int, Base) -> String when
      Int :: integer(),
      Base :: [2..36],
      String :: string().
integer_to_list(Integer, Base) -> erlang:integer_to_list(Integer, Base).

%% @doc Converts a float to a string
%% <br/>
%% <b>Example:</b>
%% ```
%% 1> sstr:float_to_list(1.0).
%% "1.0"
%% '''
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/erlang.html#float_to_list-1 erlang:float_to_list/1], 
%% [http://erlang.org/doc/man/erlang.html#float_to_list-2 erlang:float_to_list/2]. 
%%
%% @param Float float value
%% @returns string

-spec float_to_list(Float) -> String when
      Float :: float(),
      String :: string().
float_to_list(Float) -> 
    try
    erlang:float_to_list(Float,[{decimals,253},compact])
    catch
        error:badarg -> 
            float_to_list_scientific(Float,4)
    end.

%% @doc Converts a float to a string using scientific option.
%% @private
%% @param Float float value
%% @param ScientificDecimals scientific decimal value
%% @returns string that represents float in scientific way.
%%
%% This function used for beautifying scientific string

float_to_list_scientific(Float, 0) -> erlang:float_to_list(Float,[{scientific, 0}]);
float_to_list_scientific(Float, ScientificDecimals) ->
    Result = erlang:float_to_list(Float,[{scientific, ScientificDecimals}]),
    case sstr:str(Result,"00e") of
        {error, badarg} -> 
            Result;    
        _ -> float_to_list_scientific(Float,ScientificDecimals-1)
    end.


%% @doc Converts a fun to a string
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/erlang.html#fun_to_list-1 erlang:fun_to_list/1]. 
%% @param Fun float value
%% @returns string that represents the code that created Fun.

-spec fun_to_list(Fun) -> String when
      Fun :: function(),
      String :: string().
fun_to_list(Fun) -> erlang:fun_to_list(Fun).

%% @doc Converts a string to an atom.
%% <blockquote>
%% As from Erlang/OTP 20, String may contain any Unicode character. 
%% </blockquote>
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/erlang.html#list_to_atom-1 erlang:list_to_atom/1]. 
%% @param String input string
%% @returns string that represents the code that created Fun.

-spec list_to_atom(String) -> atom() when
      String :: string().
list_to_atom(String) -> erlang:list_to_atom(String).

%% @doc Compiles and executes a regular expression matching.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/re.html#compile-2 re:compile/2], 
%% [http://erlang.org/doc/man/re.html#run-3 re:run/3]. 
%% @param Subject input string
%% @param RegexString regular expression
%% @param CompileOtions regular expression compilation options
%% @param RunOptions regular expression run options
%% @returns string that represents the result of the regular expresion execution.

-spec run(Subject, RegexString, CompileOtions, RunOptions) -> Result when
      Subject :: string(),
	  RegexString :: string(), 
	  CompileOtions :: [CompileOpt], 
	  CompileOpt :: compile_option(),
	  RunOptions :: [RunOption],
	  RunOption :: 
    anchored | global | notbol | noteol | notempty |
    notempty_atstart | report_errors |
    {offset, pos_integer()} |
    {match_limit, pos_integer()} |
    {match_limit_recursion, pos_integer()} |
    {newline, NLSpec :: nl_spec()} |
    bsr_anycrlf | bsr_unicode |
    CompileOpt,
	Result :: string() | nomatch.
	
run(Subject, RegexString, CompileOtions, RunOptions)	when is_list(CompileOtions), is_list(RunOptions) ->
	{ok, MP} = re:compile(RegexString, CompileOtions),
	RunResult = re:run(Subject, MP, RunOptions ++ [{capture, all, list}]),
	case RunResult of  
	   {match, [Result]} -> Result;
	   nomatch ->  nomatch
	end.
	
%% @doc Compiles and replaces the matched part of the Subject string with the contents of Replacement.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/re.html#run-3 re:replace/3]. 
%% @param Subject input string
%% @param Replacement replace string
%% @param RegexString regular expression
%% @param CompileOtions regular expression compilation options
%% @param ReplaceOptions regular expression replace options
%% @returns string that represents the result of the replace expresion execution.

-spec replace(Subject, Replacement, RegexString, CompileOtions, ReplaceOptions) -> Result when
      Subject :: string(),
	  Replacement :: string(),
	  RegexString :: string(),
	  CompileOtions :: [CompileOpt], 
	  CompileOpt :: compile_option(),
	  ReplaceOptions :: [ReplaceOption],
	  ReplaceOption :: 
    anchored | global | notbol | noteol | notempty |
    notempty_atstart |
    {offset, pos_integer()} |
    {newline, NLSpec} |
    bsr_anycrlf |
    {match_limit, pos_integer()} |
    {match_limit_recursion, pos_integer()} |
    bsr_unicode |
    CompileOpt,
    Result :: list,
    NLSpec :: nl_spec().
	
replace(Subject, Replacement, RegexString, CompileOtions, ReplaceOptions)	when is_list(CompileOtions), 
                                                                                   is_list(ReplaceOptions) ->
	{ok, MP} = re:compile(RegexString, CompileOtions),
	Result = re:replace(Subject, MP, Replacement, ReplaceOptions ++ [{return, list}]),
	Result.

%% @doc Splits the input into parts by finding tokens according to the regular expression supplied.
%% <br/>
%% <b>See also:</b> 
%% [http://erlang.org/doc/man/re.html#split-3 re:split/3]. 
%% @param Subject input string
%% @param RegexString regular expression
%% @param CompileOtions regular expression compilation options
%% @param SplitOtions split execution options
%% @returns string that represents the result of the replace split execution.

-spec split(Subject, RegexString, CompileOtions, SplitOtions) -> SplitList when
      Subject :: string(),
	  RegexString :: string(),
	  CompileOtions :: [CompileOpt], 
	  CompileOpt :: compile_option(),
	  SplitOtions :: [SplitOtion],
	  SplitOtion :: 
    anchored | notbol | noteol | notempty | notempty_atstart |
    {offset, pos_integer()} |
    {newline, nl_spec()} |
    {match_limit, pos_integer()} |
    {match_limit_recursion, pos_integer()} |
    bsr_anycrlf | bsr_unicode |
    {parts, NumParts} |
    group | trim | CompileOpt,
    NumParts :: pos_integer() | infinity,
    CompileOpt :: compile_option(),
    SplitList :: [RetData] | [GroupedRetData],
    GroupedRetData :: [RetData],
    RetData :: iodata() | unicode:charlist() | binary() | list().
	
split(Subject, RegexString, CompileOtions, SplitOtions) when is_list(CompileOtions), is_list(SplitOtions) -> 
	{ok, MP} = re:compile(RegexString, CompileOtions),
	SplitList = re:split(Subject, MP, SplitOtions ++ [{return, list}]), 
	SplitList.

%% @doc Convert integer value to the hex-string with #-prefix 
%% @returns Return hex-string.

-spec hex(Integer) -> Result
    when Integer :: pos_integer(),
         Result :: string().
hex(Integer) ->
    String = io_lib:fwrite("~.16X", [Integer,"#"]),
    String.