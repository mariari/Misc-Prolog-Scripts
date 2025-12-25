:- module(day2, [solve_day2/3, parse_input/3]).

:- use_module(aoc(aoc_common)).
:- autoload(library(dcg/basics), [integer/3]).
:- autoload(library(dcg/high_order), [sequence/5]).
:- autoload(library(lists), [append/3]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(backcomp), [sumlist/2]).
:- autoload(library(aggregate), [aggregate_all/3]).

parse_input(Ranges) -->
    sequence(range, ",", Ranges),
     ( "\n" ; [] ).

range((R1, R2)) -->
    integer(R1), "-", integer(R2).

% This also works, introduces 1 choice point though.
parse_input_bad([(R1, R2)]) --> range((R1, R2)).
parse_input_bad([(R1, R2) | Ranges]) -->
    range((R1, R2)), ",", parse_input_bad(Ranges).

% Answer written for part1, generalized in replicate_list, in common.
split(L, FirstHalf, SecondHalf) :-
    length(L, N),
    Half is N div 2,
    length(FirstHalf, Half), length(SecondHalf, Half),
    append(FirstHalf, SecondHalf, L).

repeat_pattern(R1, R2, Number) :-
    between(R1, R2, Number),
    number_codes(Number, CodeList),
    repeated(2, CodeList).
    % split(CodeList, L1, L1).

repeat_patterns(R1, R2, Number) :-
    between(R1, R2, Number),
    number_codes(Number, CodeList),
    length(CodeList, L),
    % Once makes it so we don't find another N such that this holds,
    % causing an issue
    once(
        (between(2, L, N),
         repeated(N, CodeList))). % replicate_list(_, N, CodeList)

% Allocates a list, thus is slower, keeping for learning
answer_part1_slow((R1, R2), N) :-
    findall(X, repeat_pattern(R1, R2, X), Bag),
    sumlist(Bag, N).

answer_part1((R1, R2), N) :-
    aggregate_all(sum(X), repeat_pattern(R1, R2, X), N).

answer_part2((R1, R2), N) :-
    aggregate_all(sum(X), repeat_patterns(R1, R2, X), N).

solve_day2(part1, Input, Answer) :-
    maplist(answer_part1, Input, New),
    sumlist(New, Answer).

solve_day2(part2, Input, Answer) :-
    maplist(answer_part2, Input, New),
    sumlist(New, Answer).

% ?- phrase_from_file(day2:parse_input(_Ranges), './AOC 2025/day2/input'),
%    time(solve_day2(_, _Ranges, X)).
% X = 37314786486 ;
% X = 47477053982.

% Speed of first run
% % 28,994,016 inferences, 1.027 CPU in 1.029 seconds (100% CPU, 28237734 Lips)
% % 732,337,010 inferences, 16.682 CPU in 16.716 seconds (100% CPU, 43899898 Lips)
% After some optimizations
% % 40,630,475 inferences, 1.114 CPU in 1.116 seconds (100% CPU, 36463751 Lips)
% % 175,708,050 inferences, 4.427 CPU in 4.435 seconds (100% CPU, 39688094 Lips)

% phrase_from_file(day2:parse_input(Ranges), './AOC 2025/day2/test').
% Can run in reverse
% ?- phrase(day2:parse_input([(11,22), (95, 115)]), Ls).
% Ls = `11-22,95-115`.
