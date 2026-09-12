:- module(pcap, []).

:- use_module(library(pio)).


ipv4(Src, Dst, TTL, Proto, Len, Transport) -->
    ip_first_byte(4, IHL),
    % We don't care about DSCP and ECN
    skip(1), [B2, B3], {b16le(B2, B3, Len)},
    % we don't care about Identification and Flags for now.
    skip(4), [TTL, Proto],
    % we don't care about the checksum
    skip(2), ip4addr(Src), ip4addr(Dst),
    % Idk what to do with option length
    { OptionsLength #= (IHL - 5) * 4}, skip(OptionsLength),
    transport(Proto, Transport).

transport(Proto, Data) --> skip(1).

ipv6(Src, IHL) -->
    ip_first_byte(6, IHL).

ip_first_byte(Version, IHL) -->
    [B0], {ip_firstbyte(B0, Version, IHL)}.

ip4addr(ip(X0, X1, X2, X3)) --> [X0, X1, X2, X3].

ip_firstbyte(B0, Version, IHL) :-
    Version #= B0 << 8,
    IHL     #= B0 /\ 0xf.

b16le(B2, B3, Number) :- Number #= B2 >> 4 + B3.

% Taken do better
skip(N, S0, S) :-
    length(Pre, N), append(Pre, S, S0).

