:- module(day2, [solve_day2/3]).

:- autoload(library(dcg/basics), [integer/3]).
:- autoload(library(dcg/high_order), [sequence/5]).
:- autoload(library(lists), [append/3, append/2]).
:- autoload(library(apply), [maplist/3, maplist/2]).
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

% Answer written for part1
split(L, FirstHalf, SecondHalf) :-
    length(L, N),
    Half is N div 2,
    length(FirstHalf, Half), length(SecondHalf, Half),
    append(FirstHalf, SecondHalf, L).

% Same as split for N = 2. 2x slower than split
replicate_list(L1, N, LRepeat) :-
    length(LLRepeat, N),
    maplist(=(L1), LLRepeat),
    append(LLRepeat, LRepeat).

% Version of replicate_list, for just checking. Avoids allocating
% intermediate lists. Main benefit is that this just checks so we
% speed up from 16 seconds to 4 seconds.
% GPT gave this suggestion and code for speeding up things
repeated(N, Codes) :-
    length(Codes, L),
    0 is L mod N, BlockLen is L // N,
    length(Block, BlockLen), length(Blocks, N),
    maplist(=(Block), Blocks),
    append(Blocks, Codes).

repeat_pattern(R1, R2, Number) :-
    between(R1, R2, Number),
    number_codes(Number, CodeList),
    repeated(2, CodeList).
    % split(CodeList, L1, L1).

repeat_patterns(R1, R2, Number) :-
    between(R1, R2, Number),
    number_codes(Number, CodeList),
    length(CodeList, L),
    once(
        (between(2, L, N),
        % replicate_list(_, N, CodeList)
        repeated(N, CodeList)
        )).

answer_part1((R1, R2), N) :-
    aggregate_all(sum(X), repeat_pattern(R1, R2, X), N).

answer_part2((R1, R2), N) :-
    aggregate_all(sum(X), repeat_patterns(R1, R2, X), N).

% Allocates a list, thus is slower, keeping for learning
answer_part1_slow((R1, R2), N) :-
    findall(X, repeat_pattern(R1, R2, X), Bag),
    sumlist(Bag, N).

solve_day2(part1, Input, Answer) :-
    maplist(answer_part1, Input, New),
    sumlist(New, Answer).

solve_day2(part2, Input, Answer) :-
    maplist(answer_part2, Input, New),
    sumlist(New, Answer).

% First Run with the speed
% ?- phrase_from_file(day2:parse_input(_Ranges), './AOC 2025/day2/input'),
%    time(solve_day2(_, _Ranges, X)).
% % 28,994,016 inferences, 1.027 CPU in 1.029 seconds (100% CPU, 28237734 Lips)
% X = 37314786486 ;
% % 732,337,010 inferences, 16.682 CPU in 16.716 seconds (100% CPU, 43899898 Lips)
% X = 47477053982.

% After some optimizations
% ?- phrase_from_file(day2:parse_input(_Ranges), './AOC 2025/day2/input'),
%    time(solve_day2(_, _Ranges, X)).
% % 40,630,475 inferences, 1.114 CPU in 1.116 seconds (100% CPU, 36463751 Lips)
% X = 37314786486 ;
% % 175,708,050 inferences, 4.427 CPU in 4.435 seconds (100% CPU, 39688094 Lips)
% X = 47477053982 .

% phrase_from_file(day2:parse_input(Ranges), './AOC 2025/day2/test').
% Can run in reverse
% ?- phrase(day2:parse_input([(11,22), (95, 115)]), Ls).
% Ls = `11-22,95-115`.
