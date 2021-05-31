%% @hidden
-module(sstr_tests).
-include_lib("eunit/include/eunit.hrl").

length_test_() ->
  [
    ?_assertEqual(5, sstr:length("étude")),
    ?_assertEqual(6, sstr:length("etiuda")),
    ?_assertEqual(4, sstr:length("etűd")),
    ?_assertEqual(4, sstr:length("этюд")),
    ?_assertEqual(6, sstr:length("էտյուդ")),
    ?_assertEqual(3, sstr:length("এটুড")),
    ?_assertEqual(6, sstr:length("ეტიუდი"))
  ].

len_test_() ->
  [
    ?_assertEqual(5, sstr:len("étude")),
    ?_assertEqual(6, sstr:len("etiuda")),
    ?_assertEqual(4, sstr:len("etűd")),
    ?_assertEqual(4, sstr:len("этюд")),
    ?_assertEqual(6, sstr:len("էտյուդ")),
    % wrong result is here
    ?_assertEqual(4, sstr:len("এটুড")),
    ?_assertEqual(6, sstr:len("ეტიუდი"))
  ].

equal_test_() ->
  [
    ?_assert(sstr:equal("étude","étude")),
    ?_assert(sstr:equal("etiuda","etiuda")),
    ?_assert(sstr:equal("etűd","etűd")),
    ?_assert(sstr:equal("этюд","этюд")),
    ?_assert(sstr:equal("էտյուդ","էտյուդ")),
    ?_assert(sstr:equal("এটুড","এটুড")),
    ?_assert(sstr:equal("ეტიუდი","ეტიუდი"))
  ].

concat_1_test_() ->
  [
    ?_assertEqual("étudeétude", sstr:concat(["étude","étude"])),
    ?_assertEqual("etiudaetiuda", sstr:concat(["etiuda","etiuda"])),
    ?_assertEqual("etűdetűd", sstr:concat(["etűd","etűd"])),
    ?_assertEqual("этюдэтюд", sstr:concat(["этюд","этюд"])),
    ?_assertEqual("էտյուդէտյուդ", sstr:concat(["էտյուդ","էտյուդ"])),
    ?_assertEqual("এটুডএটুড", sstr:concat(["এটুড","এটুড"])),
    ?_assertEqual("ეტიუდიეტიუდი", sstr:concat(["ეტიუდი","ეტიუდი"]))
  ].

concat_2_test_() ->
  [
    ?_assertEqual("étudeétude", sstr:concat("étude","étude")),
    ?_assertEqual("etiudaetiuda", sstr:concat("etiuda","etiuda")),
    ?_assertEqual("etűdetűd", sstr:concat("etűd","etűd")),
    ?_assertEqual("этюдэтюд", sstr:concat("этюд","этюд")),
    ?_assertEqual("էտյուդէտյուդ", sstr:concat("էտյուդ","էտյուդ")),
    ?_assertEqual("এটুডএটুড", sstr:concat(["এটুড","এটুড"])),
    ?_assertEqual("ეტიუდიეტიუდი", sstr:concat("ეტიუდი","ეტიუდი"))
  ].

nth_test_() ->
  [
    ?_assertEqual("é", sstr:nth(0,"étude")),
    ?_assertEqual("t", sstr:nth(1,"étude")),
    ?_assertEqual("u", sstr:nth(2,"étude")),
    ?_assertEqual("d", sstr:nth(3,"étude")),
    ?_assertEqual("e", sstr:nth(4,"étude")),
    ?_assertEqual({error, badarg}, sstr:nth(5,"étude")),

    ?_assertEqual("e", sstr:nth(0,"etűd")),
    ?_assertEqual("t", sstr:nth(1,"etűd")),
    ?_assertEqual("ű", sstr:nth(2,"etűd")),
    ?_assertEqual("d", sstr:nth(3,"etűd")),
    ?_assertEqual({error, badarg}, sstr:nth(5,"etűd")),

    ?_assertEqual("э", sstr:nth(0,"этюд")),
    ?_assertEqual("т", sstr:nth(1,"этюд")),
    ?_assertEqual("ю", sstr:nth(2,"этюд")),
    ?_assertEqual("д", sstr:nth(3,"этюд")),
    ?_assertEqual({error, badarg}, sstr:nth(5,"этюд")),

    ?_assertEqual("է", sstr:nth(0,"էտյուդ")),
    ?_assertEqual("տ", sstr:nth(1,"էտյուդ")),
    ?_assertEqual("յ", sstr:nth(2,"էտյուդ")),
    ?_assertEqual("ո", sstr:nth(3,"էտյուդ")),
    ?_assertEqual("ւ", sstr:nth(4,"էտյուդ")),
    ?_assertEqual("դ", sstr:nth(5,"էտյուդ")),
    ?_assertEqual({error, badarg}, sstr:nth(6,"էտյուդ")),

    ?_assertEqual("এ", sstr:nth(0,"এটুড")),
    ?_assertEqual("টু", sstr:nth(1,"এটুড")),
    ?_assertEqual("ড", sstr:nth(2,"এটুড")),
    ?_assertEqual({error, badarg}, sstr:nth(4,"এটুড")),

    ?_assertEqual("ე", sstr:nth(0,"ეტიუდი")),
    ?_assertEqual("ტ", sstr:nth(1,"ეტიუდი")),
    ?_assertEqual("ი", sstr:nth(2,"ეტიუდი")),
    ?_assertEqual("უ", sstr:nth(3,"ეტიუდი")),
    ?_assertEqual("დ", sstr:nth(4,"ეტიუდი")),
    ?_assertEqual("ი", sstr:nth(5,"ეტიუდი")),
    ?_assertEqual({error, badarg}, sstr:nth(6,"ეტიუდი"))

  ].

hd_test_() ->
  [
    ?_assertEqual("é", sstr:hd("étude")),
    ?_assertEqual("e", sstr:hd("etűd")),
    ?_assertEqual("э", sstr:hd("этюд")),
    ?_assertEqual("է", sstr:hd("էտյուդ")),
    ?_assertEqual("এ", sstr:hd("এটুড")),
    ?_assertEqual("ე", sstr:hd("ეტიუდი"))
  ].

chr_test_() ->
  [
    ?_assertEqual(0, sstr:chr("étude","é")),
    ?_assertEqual(1, sstr:chr("étude","t")),
    ?_assertEqual(2, sstr:chr("étude","u")),
    ?_assertEqual(3, sstr:chr("étude","d")),
    ?_assertEqual(4, sstr:chr("étude","e")),
    ?_assertEqual({error, badarg}, sstr:chr("étude","k"))
  ].
%

rchr_test_() ->
  [
    ?_assertEqual(4, sstr:rchr("étudtee","t")),
    ?_assertEqual(0, sstr:rchr("étude","é")),
    ?_assertEqual(1, sstr:rchr("étude","t")),
    ?_assertEqual(2, sstr:rchr("étude","u")),
    ?_assertEqual(3, sstr:rchr("étude","d")),
    ?_assertEqual(4, sstr:rchr("étude","e")),
    ?_assertEqual(6, sstr:rchr("eétudeeqq","e")),
    ?_assertEqual({error, badarg}, sstr:rchr("étude","k"))
  ].


substr_test_() ->
  [
    ?_assertEqual(<<"ö Wörld"/utf8>>, sstr:substr(<<"He̊llö Wörld"/utf8>>, 4)),
    ?_assertEqual("étude", sstr:substr("étude",0)),
    ?_assertEqual("tude", sstr:substr("étude",1)),
    ?_assertEqual("ude", sstr:substr("étude",2)),
    ?_assertEqual("de", sstr:substr("étude",3)),
    ?_assertEqual("e", sstr:substr("étude",4)),
    ?_assertEqual("", sstr:substr("étude",5)),
    ?_assertEqual("", sstr:substr("étude",6))
  ].

substr_length_test_() ->
  [
    ?_assertEqual(<<"ö Wörld"/utf8>>, sstr:substr(<<"He̊llö Wörld"/utf8>>, 4)),
    ?_assertEqual("", sstr:substr("étude",0,0)),
    ?_assertEqual("é", sstr:substr("étude",0,1)),
    ?_assertEqual("ét", sstr:substr("étude",0,2)),
    ?_assertEqual("étu", sstr:substr("étude",0,3)),
    ?_assertEqual("étud", sstr:substr("étude",0,4)),
    ?_assertEqual("étude", sstr:substr("étude",0,5)),
    ?_assertEqual("étude", sstr:substr("étude",0,6))
  ]
.

span_test_() ->
 [
   ?_assertEqual(3, sstr:span("abc0z123", lists:seq($a,$z))),
   ?_assertEqual(3, sstr:span(<<"abc0z123">>, lists:seq($a,$z))),
   ?_assertEqual(0, sstr:span("abc0z123", lists:seq($0,$9))),
   ?_assertEqual(0, sstr:span(<<"abc0z123">>, lists:seq($0,$9))),
   
   ?_assertEqual(0, sstr:span("", ["abc"])),
   fun() ->
     Str = "\t\s..Ha\s.llå..\t\n\r",
    %  {Result,Rest} = string:take(Str, ["x"]),
    %  ?debugFmt("~n~p~n ~n~p~n", [Result,Rest])
     ?assertEqual(0, sstr:span(Str, ["x"]))
   end,
   ?_assertEqual(4, sstr:span("1234abc0z123", lists:seq($0,$9)))
  ].

cpan_test_() ->
 [
   ?_assertEqual(0, sstr:cspan("\t    abcdef", " \t"))

 ].

tokens_test_() ->
 [
   ?_assertEqual(["abc", "def", "ghi", "jkl"], sstr:tokens("abc defxxghix jkl", "x ")),
   ?_assertEqual(["abc","de̊f","ghi","jkl","foo"], sstr:tokens("abc de̊fxxghix jkl\r\nfoo", "x e" ++ [[$\r,$\n]])),
   ?_assertEqual([<<"abc">>, <<"de̊f"/utf8>>, <<"ghi">>, <<"jkl\r\nfoo">>], sstr:tokens(<<"abc de̊fxxghix jkl\r\nfoo"/utf8>>, "x e" ++ [$\r,$\n])),
   ?_assertEqual([<<"abc">>, <<"de̊f"/utf8>>, <<"ghi">>, <<"jkl">>, <<"foo">>], sstr:tokens(<<"abc de̊fxxghix jkl\r \nfoo"/utf8>>, "x e" ++ [$\r,$\n]))
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:tokens(<<"abc de̊fxxghix jkl\r\nfoo"/utf8>>, "x e" ++ [$\r] ++ " w " ++ [$\n])])
  %  end
 ].

join_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:join(["one", "two", "three"], ", ")])
  %  end
   ?_assertEqual("one, two, three", sstr:join(["one", "two", "three"], ", "))
 ].

words_1_test_() ->
[
   ?_assertEqual(3, sstr:words("one       two         three")),
   ?_assertEqual(4, sstr:words("one\ntwo\rthree four"))
].

words_2_test_() ->
[
   ?_assertEqual(4, sstr:words(" Hello old boy!", [$o]))
].

chars_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:chars("A", 7)])
  %  end
   ?_assertEqual("AAAAAAA", sstr:chars("A", 7))
].

chars_tail_test_() ->
[
   ?_assertEqual("AAAAAAA. Yes!", sstr:chars("A", 7,". Yes!"))
].

copies_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:copies("A", 7)])
  %  end
   ?_assertEqual("AAAAAAA", sstr:copies("A", 7))
].

strip_test_() ->
[
   ?_assertEqual("Hello", sstr:strip("\r   Hello     \n")),
   ?_assertEqual("Hello", sstr:strip("\r   Hello")),
   ?_assertEqual("Hello", sstr:strip("   Hello     \n"))
].

strip_2_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:strip("   Hello     \n", both)])
  %  end
   ?_assertEqual("Hello", sstr:strip("\r   Hello     \n", both)),
   ?_assertEqual("\r   Hello", sstr:strip("\r   Hello     \n", trailing)),
   ?_assertEqual("Hello     \n", sstr:strip("\r   Hello     \n", leading))
].

strip_3_test_() ->
[
   ?_assertEqual("\r   Hello     ", sstr:strip("\r   Hello     \n", both, "\n")),
   ?_assertEqual("\r   Hello     ", sstr:strip("\r   Hello     \n", trailing, "\n")),
   ?_assertEqual("   Hello     \n", sstr:strip("\r   Hello     \n", leading, "\r")),
   ?_assertEqual("   Hello     \n", sstr:strip("\r   Hello     \n", leading, "\r\n")),
   ?_assertEqual("   Hello     ", sstr:strip("\r   Hello     \n", both, "\r\n")),
   ?_assertEqual(<<".Hello">>, sstr:strip(<<".Hello.\n">>, trailing, "\n."))
].

left_2_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:left("Hello", 10)])
  %  end
   ?_assertEqual("     Hello", sstr:left("Hello", 10))
].  

left_3_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:left("Hello", 10, $.)])
  %  end
   ?_assertEqual(".....Hello", sstr:left("Hello", 10, $.))
].  

right_2_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:right("Hello", 10)])
  %  end
   ?_assertEqual("Hello     ", sstr:right("Hello", 10))
].  

right_3_test_() ->
[
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:right("Hello", 10, $.)])
  %  end
   ?_assertEqual("Hello.....", sstr:right("Hello", 10, $.))
].  


centre_2_test_() ->
[
  %   fun() ->
  %   ?debugFmt("~n~p~n", [sstr:centre("Hello", 10)])
  %  end
  ?_assertEqual("  Hello   ", sstr:centre("Hello", 10)) 
].  

centre_3_test_() ->
[
  ?_assertEqual("..Hello...", sstr:centre("Hello", 10, $.))
].  

reverse_test_() ->
  %   fun() ->
  %   ?debugFmt("~n~ts~n", [sstr:reverse("étude")])
  %  end.
[
  ?_assertEqual("ÖÄÅ", sstr:reverse("ÅÄÖ")),
  ?_assertEqual("eduté", sstr:reverse("étude")),
  ?_assertEqual("дютэ", sstr:reverse("этюд"))    
].

reverse_tail_test_() ->
  %   fun() ->
  %   ?debugFmt("~n~ts~n", [sstr:reverse("ÅÄÖ", " - ok")])
  %  end.
[
  ?_assertEqual("ÖÄÅ - ok", sstr:reverse("ÅÄÖ", " - ok")),
  ?_assertEqual("eduté - ok", sstr:reverse("étude", " - ok")),
  ?_assertEqual("дютэ - ok", sstr:reverse("этюд", " - ok"))    
].

to_float_test_() ->
  %  fun() ->
  %   ?debugFmt("~n~p~n", [sstr:to_float("-1.5eX")])
  %  end.  
[
?_assertEqual(0.9, sstr:to_float("0.9")),
?_assertEqual(1.528535047e-25, sstr:to_float("1.528535047E-25")),
?_assertEqual(-3.58e7, sstr:to_float("-3.58E7")),

?_assertEqual({error,no_float}, sstr:to_float("-128")),
?_assertEqual(-128.0, sstr:to_float("-128.0")),
?_assertEqual({error,no_float}, sstr:to_float("127")),
?_assertEqual(127.0, sstr:to_float("127.0")),
?_assertEqual(-32768.0, sstr:to_float("-32768.0")),
?_assertEqual(32767.0, sstr:to_float("32767.0")),
?_assertEqual(2147483647.0, sstr:to_float("2147483647.0")),
?_assertEqual(-9.223372036854776e18, sstr:to_float("-9223372036854775808.0")),
?_assertEqual(9.223372036854776e18, sstr:to_float("9223372036854775807.0")),
?_assertEqual(-3.4e38, sstr:to_float("-3.4E+38")),
?_assertEqual(3.4e38, sstr:to_float("3.4E+38")),
?_assertEqual(-1.7e308, sstr:to_float("-1.7E+308")),
?_assertEqual(1.7e308, sstr:to_float("1.7E+308")),

?_assertEqual(1.0, sstr:to_float("1.0-1.0e-1")),
?_assertEqual(-0.1, sstr:to_float("-1.0e-1")),
?_assertEqual({error,no_float}, sstr:to_float("3/2=1.5")),
?_assertEqual(-1.5, sstr:to_float("-1.5eX"))
].

to_integer_test_() ->
  % fun() ->
  %   ?debugFmt("~n~p~n", [sstr:to_integer("010,00")])
  % end.  
[
  ?_assertEqual(33, sstr:to_integer("33")),
  ?_assertEqual(22, sstr:to_integer("+22")),
  ?_assertEqual(0, sstr:to_integer("0.5")),
  ?_assertEqual({error,no_integer}, sstr:to_integer("x=2"))
].

to_lower_test_() ->
  % fun() ->
  %   ?debugFmt("~n~ts~n", [sstr:to_lower("АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")])
  % end.  
[
  ?_assertEqual("michał", sstr:to_lower("MICHAŁ")),
  ?_assertEqual("ęąśżźćńłó", sstr:to_lower("ĘĄŚŻŹĆŃŁÓ")),
  ?_assertEqual("абвгдеёжзийклмнопрстуфхцчшщъыьэюя", sstr:to_lower("АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))

].

to_upper_test_() ->
  % fun() ->
  %   ?debugFmt("~n~ts~n", [sstr:to_upper("michał")])
  % end.  
[
  ?_assertEqual("MICHAŁ", sstr:to_upper("michał")),
  ?_assertEqual("ĘĄŚŻŹĆŃŁÓ", sstr:to_upper("ęąśżźćńłó")),
  ?_assertEqual("АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", sstr:to_upper("абвгдеёжзийклмнопрстуфхцчшщъыьэюя"))

].

integer_to_list_test_() ->
  % fun() ->
  %   ?debugFmt("~n~p~n", [sstr:integer_to_list("1")])
  % end.  
[
  ?_assertEqual("1", sstr:integer_to_list(1)),
  ?_assertEqual("-1", sstr:integer_to_list(-1)),
  ?_assertEqual("1", sstr:integer_to_list(+1)),
  ?_assertError(badarg, sstr:integer_to_list("1"))
].

integer_to_list_2_test_()->
  % fun() ->
  %   ?debugFmt("~n~p~n", [sstr:integer_to_list(36,36)])
  % end.  
[
  ?_assertEqual("1", sstr:integer_to_list(1,2)),
  ?_assertEqual("1", sstr:integer_to_list(1,36)),
  ?_assertError(badarg, sstr:integer_to_list(1,1)),
  ?_assertError(badarg, sstr:integer_to_list(1,37)),
  ?_assertEqual("10", sstr:integer_to_list(36,36))
].

float_to_list_test_() ->
  % fun() ->
  %   ?debugFmt("~n~p~n", [sstr:float_to_list(-3.4E+38)])
  % end.    
  [
  ?_assertEqual("1.0", sstr:float_to_list(1.0)),
  ?_assertEqual("9.2234e+18", sstr:float_to_list(9223372036854775807.0)),  
  ?_assertEqual("-9.2234e+18", sstr:float_to_list(-9223372036854775808.0)),  
  ?_assertEqual("-3.40e+38", sstr:float_to_list(-3.4E+38)),  
  ?_assertEqual("-1.0e-01", sstr:float_to_list(-1.0e-1)),
  ?_assertEqual("-1.70e+308", sstr:float_to_list(-1.7E+308))
].

fun_to_list_test_() ->
  % fun() ->
  %   Fun = fun(A) -> A * A end,
  %   ?debugFmt("~n~p~n", [sstr:fun_to_list(Fun)])
  % end.
[
  ?_assertEqual("fun sstr:fun_to_list/1", sstr:fun_to_list(fun sstr:fun_to_list/1)),
  fun() ->
    Fun = fun(A) -> A * A end,
    Result = sstr:fun_to_list(Fun),
    ?assertEqual(0,  sstr:chr(Result, "#"))
  end  
].

list_to_atom_test_()->
  % fun() ->
  %   ?debugFmt("~n~p~n", [sstr:list_to_atom("абвгдеёжзийклмнопрстуфхцчшщъыьэюя")])
  % end.
  % fun() ->
  %   ?debugFmt("~n~ts~n", [sstr:list_to_atom("абвгдеёжзийклмнопрстуфхцчшщъыьэюя")])
  % end.
[
  ?_assertEqual(ok, sstr:list_to_atom("ok")),
  ?_assertEqual('micha\x{142}', sstr:list_to_atom("michał")),
  ?_assertEqual('\x{430}\x{431}\x{432}\x{433}\x{434}\x{435}\x{451}\x{436}\x{437}\x{438}\x{439}\x{43A}\x{43B}\x{43C}\x{43D}\x{43E}\x{43F}\x{440}\x{441}\x{442}\x{443}\x{444}\x{445}\x{446}\x{447}\x{448}\x{449}\x{44A}\x{44B}\x{44C}\x{44D}\x{44E}\x{44F}',sstr:list_to_atom("абвгдеёжзийклмнопрстуфхцчшщъыьэюя"))

].