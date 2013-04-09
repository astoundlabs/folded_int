%% @author    David Cuddeback <david@astoundlabs.com>
%% @copyright 2013 David Cuddeback
%% @version   {@version}
%%
%% @doc Handles the encoding and decoding of folded integers in binary strings. A folded integer is
%% an integer that is encoded into a variable-length byte sequence, depending on its value. Smaller
%% values consume less bytes.
%%
%% == Format ==
%%
%% The format of a folded integer is a variable number of segment bytes. Each segment byte consists
%% of one carry bit and seven value bits. The carry bit is the high-order bit and the value is
%% stored in the seven least significant bits. If written as a C struct, a segment byte would look
%% like this:
%%
%% ```
%% typedef struct {
%%   union {
%%     struct {
%%       unsigned value:7;
%%       unsigned carry:1;
%%     };
%%     uint8_t bits;
%%   };
%% } fi_segment_t;
%% '''
%%
%% To encode an integer, the integer is broken up into seven-bit segments and written in
%% little-endian byte order (LSB first). Each byte, except for the last one, will have the carry bit
%% set. The last byte (containing the most significant bits) does not have the carry bit set, which
%% demarcates the end of a folded integer in a binary.
%%
%% === Encoding Example ===
%%
%% To help understand the format of a folded integer, here is an example of encoding the value
%% `16#DEADBEEF' as a folded integer.
%%
%%  <ol>
%%    <li>
%%      Begin by considering the value's binary representation:
%%
%%      <pre>11011110 10101101 10111110 11101111</pre>
%%   </li>
%%   <li>
%%      Next, divide the value into seven-bit segments:
%%
%%      <pre><b>000</b>1101 1110101 0110110 1111101 1101111</pre>
%%
%%      The high-order bits need to be padded with zeros (shown in bold) to make each segment
%%      contain seven bits.
%%   </li>
%%   <li>
%%      Before being written to a binary, carry bits (shown in bold) must be added to each segments.
%%      Note that each segment except for the one containing the high-order bits has its carry bit
%%      set.
%%
%%      <pre><b>0</b>0001101 <b>1</b>1110101 <b>1</b>0110110 <b>1</b>1111101 <b>1</b>1101111</pre>
%%   </li>
%%   <li>
%%      The bytes have to be reversed before being serialized in order match the little-endian
%%      format:
%%
%%      <pre>11101111 11111101 10110110 11110101 00001101</pre>
%%
%%      All that's left to do is send the bytes whereever they need to be serialized.
%%    </li>
%% </ol>
%%
%% In the above example, `16#DEADBEEF' serializes to `16#EFFDB6F50D', which consumes an extra byte
%% when compared to its 32-bit representation (but three less bytes when compared to its 64-bit
%% representation).  Smaller numbers, such as `42' consume less space than their 32- or 64-bit
%% representations. In fact, any number smaller than `2,097,152' will consume less space than a full
%% 32-bit integer.
%%
%%
%% == Limitations ==
%%
%% The `folded_int' module currently only handles unsigned integers. To use signed integers, you
%% must map them to unsigned integers using something like zig-zag encoding. There is also no
%% attempt to protect against malicious binary strings, such as an endless sequence of bit segments
%% with the carry bits set.

-module(folded_int).
-author("David Cuddeback <david@astoundlabs.com>").

-export([consume/1, encode/1, decode/1]).


%% @doc Consumes a folded integer from a binary string.
%%
%% It returns a tuple consisting of the decoded integer and the rest of the binary string. For
%% example:
%%
%%    ```
%%    {Int, Tail} = folded_int:consume(<<0:1, 42:7, "foo">>).
%%    Int.  % => 42
%%    Tail. % => <<"foo">>
%%    '''
%%
%% If a folded integer can't be consumed from the binary string, then it returns `false'. This
%% happens when the binary string is empty (`<<>>') or contains an incomplete folded integer (all
%% the carry bits are set).  If the binary string contains exactly one folded integer with no extra
%% bytes, the return value will be the decoded integer and an empty binary string (`{N, <<>>}').
%%
%% <strong>Warning:</strong> No attempt is made to protect against overflows in the case of a long
%% string of bytes with set carry bits.

-spec consume(X::binary()) -> {pos_integer(), binary()} | false.

consume(Binary) ->
  case avail(Binary) of
    {ok, N} ->
      << Encoded:N/binary, Tail/binary >> = Binary,
      { decode(Encoded), Tail };
    false   -> false
  end.


%% @doc Encodes an integer as a variable-length binary string.
%%
%%    ```folded_int:encode(42). % => <<0:1, 42:7>>'''

-spec encode(N::pos_integer()) -> binary().

encode(N) when N < 16#80 ->
  <<0:1, N:7>>;
encode(N) ->
  LSB = N band 16#7F,
  MSB = encode(N bsr 7),
  <<1:1, LSB:7, MSB/binary>>.


%% @doc Decodes an integer from a variable-length binary string. The binary string passed as an
%% argument must be a valid folded integer. If the binary is missing any bytes or contains extra
%% bytes, `decode/1' will throw an error. If you need to consume folded integers from a binary
%% string of unknown length, use {@link consume/1} instead.
%%
%%    ```
%%    folded_int:decode(<<1:1, 0:7, 0:1, 2:7>>).  % => 256
%%    folded_int:decode(<<1:1, 0:7>>).            % => error (underflow)
%%    folded_int:decode(<<0:0, 42:7, "foo">>).    % => error (overflow)
%%    '''

-spec decode(X::binary()) -> pos_integer().

decode(<<0:1, N:7>>) -> N;
decode(<<1:1, N:7, Rest/binary>>) ->
  MSB = decode(Rest),
  (MSB bsl 7) bor N.


%% @doc Returns the number of bytes, if any, that are available to be consumed as a folded integer.
%% If a folded integer isn't available (the binary is empty or doesn't begin with a valid folded
%% integer), then it returns false. When an integer is available, it returns a tuple of {ok, N},
%% where N is the number of bytes that are available as a folded integer.
%%
%% @private

-spec avail(X::binary()) -> {ok, pos_integer()} | false.

avail(<<>>)                   -> false;
avail(<<0:1, _:7, _/binary>>) -> {ok, 1};
avail(<<1:1, _:7, T/binary>>) ->
  case avail(T) of
    {ok, N} -> {ok, N + 1};
    false   -> false
  end.
