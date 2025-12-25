:- module(aoc_common, [read_file/2, replicate_list/3, repeated/2]).
:- autoload(library(readutil), [read_line_to_codes/2]).
:- autoload(library(apply), [maplist/2]).
:- autoload(library(lists), [append/2]).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,X),
    read_file(Stream,L).

% Same as split for N = 2. 2x slower than day2:split
replicate_list(L1, N, LRepeat) :-
    length(LLRepeat, N),
    maplist(=(L1), LLRepeat),
    append(LLRepeat, LRepeat).

% Version of replicate_list, for just recognization. Recognization
% speeds up solve_day2(part2,_,_) from 16 seconds to 4 seconds.
repeated(N, Codes) :-
    length(Codes, L),
    0 is L mod N, BlockLen is L // N,
    length(Block, BlockLen), length(Blocks, N),
    maplist(=(Block), Blocks),
    append(Blocks, Codes).
