:- module(day1, [move_fun/4, input_test/1, solve_day1/3]).

:- use_module(aoc(aoc_common)).
:- use_module(library(clpfd)).
:- autoload(library(dcg/basics), [integer/3]).
:- autoload(library(apply), [foldl/4]).

% DCG effects in system
move_fun(Start, Result) --> "L", integer(X), {Result #= (Start - X) mod 100}.
move_fun(Start, Result) --> "R", integer(X), {Result #= (Start + X) mod 100}.

% DCG more properly
move(move(l, X)) --> "L", integer(X).
move(move(r, X)) --> "R", integer(X).

apply_move(Move, Start, Dial, DialFull) :-
    delta(Move, D),
    DialFull #= Start + D,
    Dial #= DialFull mod 100.

delta(move(l,X), -X).
delta(move(r,X),  X).

dial_protocol_1(X, N, M) :-
    0 #= X mod 100 #<==> B,
    M #= N + B.

dial_protocol_2(Full_Dial, Old_Normalized_Dial, Count, New_Count) :-
    Div #= abs(Full_Dial // 100),

    (Full_Dial #=< 0)           #<==> Bneg,
    (Old_Normalized_Dial #\= 0) #<==> Bnz,
    Extra #= Bneg * Bnz,        % True iff bneg ∧ bnz

    New_Count #= Count + Div + Extra.

answer_part1(Input, (Old_Dial, Old_Count), (Dial, Count)) :-
    phrase(move(M), Input),
    apply_move(M, Old_Dial, Dial, _),
    dial_protocol_1(Dial, Old_Count, Count).

answer_part2(Input, (Old_Dial, Old_Count), (Dial, Count)) :-
    phrase(move(M), Input),
    apply_move(M, Old_Dial, Dial, Dial_Full),
    dial_protocol_2(Dial_Full, Old_Dial, Old_Count, Count).

solve_day1(part1, Input, Answer) :-
    foldl(answer_part1, Input, (50, 0), (_, Answer)).
solve_day1(part2, Input, Answer) :-
    foldl(answer_part2, Input, (50, 0), (_, Answer)).

input_test([`L68`, `L30`, `R48`, `L5`, `R60`, `L55`, `L1`, `L99`, `R14`, `L82`]).

% open('./AOC 2025/day1/input', read, _Str), read_file(_Str, _Input), solve_day1(_, _Input, Answer).
% Answer = 1195;
% Answer = 6770.

% day1:input_test(_Input), solve_day1(_, _Input, Answer).


% ?- phrase(day1:move(M), `L68`), day1:apply_move(M, 50, X).
% M = move(l, 68),
% X = 82.
