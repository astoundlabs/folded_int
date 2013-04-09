%% @author    David Cuddeback <david@astoundlabs.com>
%% @copyright 2013 David Cuddeback
%% @version   {@version}
%%
%% @todo Add functions for splitting folded ints from binary streams.

-module(folded_int).
-author("David Cuddeback <david@astoundlabs.com>").

-export([encode/1, decode/1]).


%% @doc Encodes an integer as a variable-length binary string.

-spec encode(N::pos_integer()) -> binary().

encode(N) when N < 16#80 ->
  <<0:1, N:7>>;
encode(N) ->
  LSB = N band 16#7F,
  MSB = encode(N bsr 7),
  <<1:1, LSB:7, MSB/binary>>.


%% @doc Decodes an integer from a variable-length binary string.

-spec decode(X::binary()) -> pos_integer().

decode(<<0:1, N:7>>) -> N;
decode(<<1:1, N:7, Rest/binary>>) ->
  MSB = decode(Rest),
  (MSB bsl 7) bor N.
