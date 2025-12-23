:- module(aoc_common, [read_file/2]).
:- autoload(library(readutil), [read_line_to_codes/2]).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,X),
    read_file(Stream,L).
