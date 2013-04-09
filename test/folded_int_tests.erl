-module(folded_int_tests).
-author("David Cuddeback <david@astoundlabs.com>").

-include_lib("eunit/include/eunit.hrl").


%%% ============================================================================
%%% encode/1 tests
%%% ============================================================================

encode_0_test()   -> ?assertEqual(<<0:1, 0:7>>,     folded_int:encode(0)).
encode_42_test()  -> ?assertEqual(<<0:1, 42:7>>,    folded_int:encode(42)).
encode_127_test() -> ?assertEqual(<<0:1, 16#7F:7>>, folded_int:encode(16#7F)).

encode_128_test()   -> ?assertEqual(<<1:1, 16#00:7, 0:1, 16#01:7>>, folded_int:encode(16#80)).
encode_129_test()   -> ?assertEqual(<<1:1, 16#01:7, 0:1, 16#01:7>>, folded_int:encode(16#81)).
encode_255_test()   -> ?assertEqual(<<1:1, 16#7F:7, 0:1, 16#01:7>>, folded_int:encode(16#FF)).
encode_16383_test() -> ?assertEqual(<<1:1, 16#7F:7, 0:1, 16#7F:7>>, folded_int:encode(16#3FFF)).

encode_16384_test() -> ?assertEqual(<<1:1, 16#00:7, 1:1, 16#00:7, 0:1, 16#01:7>>, folded_int:encode(16#4000)).
encode_16511_test() -> ?assertEqual(<<1:1, 16#7F:7, 1:1, 16#00:7, 0:1, 16#01:7>>, folded_int:encode(16#407F)).
encode_32640_test() -> ?assertEqual(<<1:1, 16#00:7, 1:1, 16#7F:7, 0:1, 16#01:7>>, folded_int:encode(16#7F80)).

encode_deadbeef_test() ->
  ?assertEqual(
    <<1:1, 16#6F:7, 1:1, 16#7D:7, 1:1, 16#36:7, 1:1, 16#75:7, 0:1, 16#0D:7>>,
    folded_int:encode(16#DEADBEEF)
  ).

encode_max_64bit_test() ->
  ?assertEqual(
    <<1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7,
      1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 0:1, 16#01:7>>,
    folded_int:encode(16#FFFFFFFFFFFFFFFF)
).


%%% ============================================================================
%%% decode/1 tests
%%% ============================================================================

decode_0_test()   -> ?assertEqual(0,     folded_int:decode(<<0:1, 0:7>>)).
decode_42_test()  -> ?assertEqual(42,    folded_int:decode(<<0:1, 42:7>>)).
decode_127_test() -> ?assertEqual(16#7F, folded_int:decode(<<0:1, 16#7F:7>>)).

decode_128_test()   -> ?assertEqual(folded_int:decode(<<1:1, 16#00:7, 0:1, 16#01:7>>), 16#80).
decode_129_test()   -> ?assertEqual(folded_int:decode(<<1:1, 16#01:7, 0:1, 16#01:7>>), 16#81).
decode_255_test()   -> ?assertEqual(folded_int:decode(<<1:1, 16#7F:7, 0:1, 16#01:7>>), 16#FF).
decode_16383_test() -> ?assertEqual(folded_int:decode(<<1:1, 16#7F:7, 0:1, 16#7F:7>>), 16#3FFF).

decode_16384_test() -> ?assertEqual(folded_int:decode(<<1:1, 16#00:7, 1:1, 16#00:7, 0:1, 16#01:7>>), 16#4000).
decode_16511_test() -> ?assertEqual(folded_int:decode(<<1:1, 16#7F:7, 1:1, 16#00:7, 0:1, 16#01:7>>), 16#407F).
decode_32640_test() -> ?assertEqual(folded_int:decode(<<1:1, 16#00:7, 1:1, 16#7F:7, 0:1, 16#01:7>>), 16#7F80).

decode_deadbeef_test() ->
  ?assertEqual(
    folded_int:decode(<<1:1, 16#6F:7, 1:1, 16#7D:7, 1:1, 16#36:7, 1:1, 16#75:7, 0:1, 16#0D:7>>),
    16#DEADBEEF
  ).

decode_max_64bit_test() ->
  ?assertEqual(
    folded_int:decode(
      <<1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7,
        1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 1:1, 16#7F:7, 0:1, 16#01:7>>
      ),
    16#FFFFFFFFFFFFFFFF
).
