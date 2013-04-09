-module(folded_int_tests).
-author("David Cuddeback <david@astoundlabs.com>").

-include_lib("eunit/include/eunit.hrl").

-define(TITLE(Type,N), list_to_binary(io_lib:format("~s ~.16#", [Type, N]))).

-define(TEST_CASES, [
    %% Single byte encodings
    { 0,     <<0:1, 0:7>>     } ,
    { 16#42, <<0:1, 16#42:7>> } ,
    { 16#7F, <<0:1, 16#7F:7>> } ,

    %% Two-byte encodings
    { 16#80,   <<1:1, 16#00:7, 0:1, 16#01:7>> },
    { 16#81,   <<1:1, 16#01:7, 0:1, 16#01:7>> },
    { 16#FF,   <<1:1, 16#7F:7, 0:1, 16#01:7>> },
    { 16#100,  <<1:1, 16#00:7, 0:1, 16#02:7>> },
    { 16#3FFF, <<1:1, 16#7F:7, 0:1, 16#7F:7>> },

    %% Multi-byte encodings
    { 16#4000, <<1:1, 16#00:7, 1:1, 16#00:7, 0:1, 16#01:7>> },
    { 16#407F, <<1:1, 16#7F:7, 1:1, 16#00:7, 0:1, 16#01:7>> },
    { 16#7F80, <<1:1, 16#00:7, 1:1, 16#7F:7, 0:1, 16#01:7>> },

    { 16#DEADBEEF, <<1:1, 16#6F:7, 1:1, 16#7D:7, 1:1, 16#36:7, 1:1, 16#75:7, 0:1, 16#0D:7>> },

    %% uint64_t max
    { 16#FFFFFFFFFFFFFFFF, <<1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7,
                             1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 0:1, 16#01:7>> }
  ]).

consume_empty_test() ->
  ?assertEqual(false, folded_int:consume(<<>>)).

consume_exact_test_() ->
  [ {?TITLE("consume exactly", N),
      ?_assertEqual({N, <<>>}, folded_int:consume(Bits))}
    || {N, Bits} <- ?TEST_CASES ].

consume_underflow_test_() ->
  [ {?TITLE("consume underflow", N),
      ?_assertEqual(false, folded_int:consume(binary:part(Bits, 0, size(Bits) - 1)))}
    || {N, Bits} <- ?TEST_CASES ].

consume_overflow_test_() ->
  [ {?TITLE("consume with overflow", N),
      ?_assertMatch({N, _}, folded_int:consume(<<Bits/binary, 42>>))}
    || {N, Bits} <- ?TEST_CASES ].

consume_tail_test_() ->
  [ {?TITLE("consume with tail", N),
      ?_assertMatch({_, <<42>>}, folded_int:consume(<<Bits/binary, 42>>))}
    || {N, Bits} <- ?TEST_CASES ].

encode_test_() ->
  [ {?TITLE("encode", N),
      ?_assertEqual(Bits, folded_int:encode(N))}
    || {N, Bits} <- ?TEST_CASES ].

decode_test_() ->
  [ {?TITLE("decode", N),
      ?_assertEqual(N, folded_int:decode(Bits))}
    || {N, Bits} <- ?TEST_CASES ].

decode_empty_test() ->
  ?assertError(function_clause, folded_int:decode(<<>>)).

decode_underflow_test() ->
  ?assertError(function_clause, folded_int:decode(<<1:1, 42:7>>)).

decode_overflow_test() ->
  ?assertError(function_clause, folded_int:decode(<<0:1, 42:7, 21>>)).
