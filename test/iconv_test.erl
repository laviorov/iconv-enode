-module(iconv_test).

-include_lib("eunit/include/eunit.hrl").

%%%========== Test collection ============================================
iconv_test_() ->
    {setup,
     fun () -> {ok,_} = iconv:start_link() end,
     fun(_) -> iconv:stop() end,
     [{timeout, 60,{"Convert from latin-1 to utf-8", fun latin1_to_utf8/0}}
     ,{"Double-expand corruption", fun double_expand/0}
     ,{timeout, 60,{"Convert from utf-8 to latin-1 ", fun utf8_to_latin1/0}}
      %% , {"Big test", fun bigtest/0}
      %%      , {"Bad-input test", fun errortest/0}
     , [{timeout, 60,{"Round-trip test "++CS++"->utf8->"++CS, fun() -> roundtrip(list_to_binary(CS)) end}}
         || CS <- ["latin1",
                   "ISO-8859-1",
                   "ISO-8859-2",
                   "ISO-8859-3",
                   "ISO-8859-4",
                   "ISO-8859-5",
                   "ISO-8859-6",
                   "ISO-8859-7",
                   "ISO-8859-8",
                   "ISO-8859-9",
                   "ISO-8859-10",
                   "ISO-8859-11",
                   "ISO-8859-13",
                   "ISO-8859-14",
                   "ISO-8859-15",
                   "ISO-8859-16"]]]}.

%%%===============================>>=============================

test_strings() ->
    Latin1Characters = lists:seq(0,255),
    [%% Basics:
     "", "Hello, World!",
     %% Non-ASCII characters:
     "Blåbærgrød",
     "test æøå",
     "æøåÅØÆ",
     [128,255]] ++
        %% All one-character and two-character strings:
        [[X] || X <- Latin1Characters] ++
        [[X,Y] || X <- Latin1Characters, Y <- Latin1Characters] ++
        %% Random input:
        [binary_to_list(crypto:rand_bytes(X)) || X <- lists:seq(1,200)].

double_expand() ->
    latin1_to_utf8("Test æøå").

latin1_to_utf8() ->
    [latin1_to_utf8(X) || X <- test_strings()].

latin1_to_utf8(S) ->
    In = list_to_binary(S),
    Out = unicode:characters_to_binary(S, latin1),
    ?assertEqual({ok, Out}, iconv:conv(<<"ISO-8859-1">>, <<"utf-8">>, In)).


utf8_to_latin1() ->
    [utf8_to_latin1(X) || X <- test_strings()].

utf8_to_latin1(S) ->
    In = unicode:characters_to_binary(S, latin1),
    Out = list_to_binary(S),
    ?assertEqual({ok, Out}, iconv:conv(<<"utf-8">>, <<"ISO-8859-1">>, In)).

roundtrip(CS) ->
    IllegalBytes = illegal_bytes_for_encoding(CS),
    Bytes = lists:seq(0,255) -- IllegalBytes,
    TestStrings =
        %% All zero-, one-, and two-byte sequences:
        [<<>>] ++
        [<<X>> || X <- Bytes] ++
        [<<X,Y>> || X <- Bytes, Y <- Bytes] ++
        %% Random input:
        [bytes_not_in(crypto:rand_bytes(X), IllegalBytes)
         || X <- lists:seq(1,200)],
    [roundtrip(CS, <<"utf-8">>, X) || X <- TestStrings].

roundtrip(From, To, In) ->
    {ok, Tmp} = iconv:conv(From, To, In),
    ?assertEqual({ok, In}, iconv:conv(To, From, Tmp)).

bytes_not_in(Bin, Exclude) ->
    << <<X>> || <<X>> <= Bin, not lists:member(X,Exclude)>>.

illegal_bytes_for_encoding(<<"ISO-8859-3">>) -> [165,174,190,195,208,227,240];
illegal_bytes_for_encoding(<<"ISO-8859-6">>) -> lists:seq(161,163)++lists:seq(165,171)++lists:seq(174,186)++lists:seq(188,190)++[192]++lists:seq(219,223)++lists:seq(243,255);
illegal_bytes_for_encoding(<<"ISO-8859-7">>) -> [174,210,255];
illegal_bytes_for_encoding(<<"ISO-8859-8">>) -> [161]++lists:seq(191,222)++[251,252,255];
illegal_bytes_for_encoding(<<"ISO-8859-11">>) -> lists:seq(219,222)++lists:seq(252,255);
illegal_bytes_for_encoding(_) -> [].

%---
bigtest() ->
    [begin
         In = list_to_binary(string:copies("x",100*N)),
         {ok,Out} = iconv:conv(<<"utf-8">>, <<"latin1">>, In),
         %% io:format(user, "DB| ~w~n vs ~w~n", [In, iconv:conv(CD, In)]),
         ?assertMatch({N,X,X}, {N,byte_size(In), byte_size(Out)}),
         ?assertMatch({N,{ok,In}}, {N,iconv:conv(<<"utf-8">>, <<"latin1">>, In)})
     end
     || N <- lists:seq(655,1000)].

%% errortest() ->
%%     {ok, CD} = iconv:open("ISO-8859-1", "utf-8"),
%%     ?assertEqual({ok, <<>>}, iconv:conv(CD, <<>>)),
%%     ?assertEqual({error, eilseq}, iconv:conv(CD, <<2#10000000>>)),
%%     ?assertEqual({error, einval}, iconv:conv(CD, <<2#11100000>>)),
%%     ?assertEqual({error, einval}, iconv:conv(CD, <<2#11100000, 2#10000000>>)),
%%     iconv:close(CD).

%% leaktest() ->
%%     In = list_to_binary(string:copies("x",60000)),
%%     {ok, CD} = iconv:open("latin1", "utf-8"),
%%     erlang:display(erlang:memory()),
%%     [begin
%%          ?assertMatch({error,eilseq}, iconv:conv(CD, <<In/binary, 16#80>>)),
%%          ?assertMatch({error,einval}, iconv:conv(CD, <<In/binary, 16#E0>>))
%%          %% timer:sleep(1)
%%      end
%%      || _ <- lists:seq(1,600000)],
%%     erlang:display(erlang:memory()),
%%     iconv:close(CD).
