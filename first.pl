:- module(first, [factorial/2, maximum/2, maximum/3,
                  sigma/3, as/2, xy/2, qsort/2, sudoku/1, print_sudoku/1]).

:- use_module(library(clpfd)).
:- use_module(second).

:- autoload(library(lists), [member/2]).
:- autoload(library(apply), [foldl/4]).

?- member(_X, [a,b,c]).
?- color(red).
?- color(green).
?- color(blue).

factorial(0,1).
factorial(N, Res) :-
    N #> 0,
    N1 #= N - 1,
    Res #= Res2 * N,
    factorial(N1, Res2).

maximum([], 0).
maximum([L | Ls], M) :-
    M #= max(L, M1),
    maximum(Ls, M1).

maximum([], Acc, Acc).
maximum([L | Ls], M, Acc) :-
    Acc1 is max(L, Acc),
    maximum(Ls, M, Acc1).

sigma(A, B, N) :-
    findall(X, between(A, B, X), List),
    foldl(plus, List, 0, N).

as --> "".
as --> "a", as.

xy --> [].
xy --> ("X" | "Y"), xy.

append([], L, L).
append([X | L1], L2, [X | L3]) :- append(L1, L2, L3).

% Code thought from Agent Oriented Programming

% The book argues this has and parallism problems in `Sorted`. Further
% the book makes the argument that q(X,Y) :- p(X), r(Y).  May not be
% parallel as we may call q(Z,Z). However I believe there are good
% ways around this.
qsort([], []).
qsort([Item], [Item]).
qsort([Pivot, Item | Rest], Sorted) :-
    partition([Item | Rest], Pivot, Lesser, Greater),
    qsort(Lesser, LSorted),
    qsort(Greater, GSorted),
    % I always forget to add back pivot...
    append(LSorted, [Pivot | GSorted], Sorted).

% Let's single pass this, rather than doing the normal double pass of
% filter
partition([], _, [], []).
partition([X | Xs], Pivot, Ls, [X | Gs]) :-
    Pivot @=< X,
    partition(Xs, Pivot, Ls, Gs).
partition([X | Xs], Pivot, [X | Ls], Gs) :-
    X @< Pivot,
    partition(Xs, Pivot, Ls, Gs).

sudoku(Rows) :-
    length(Rows, N),
    Blocks #> 0, N #= Blocks ^ 2, indomain(Blocks),
    maplist(same_length(Rows), Rows),
    append(Rows, Cells), Cells ins 1..N,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    distinct_blocks(Rows, Blocks).

distinct_blocks([], _).
distinct_blocks(Rows, N) :-
    split(N, Rows, Firsts, Rests),
    distinct_boxs(Firsts, N),
    distinct_blocks(Rests, N).


distinct_boxs([[] | _], _).
distinct_boxs(Band, N) :-
    maplist(split(N), Band, Box, Rest),
    append(Box, Flat),
    all_distinct(Flat),
    distinct_boxs(Rest, N).

split(N, List, Firsts, Rests) :-
    length(Firsts, N),
    append(Firsts, Rests, List).

% first:puzzle(2, R), time(sudoku(R)), append(R, Cells),
% labeling([ff], Cells), first:print_sudoku(R).

% Taken from CLPFD
puzzle(1, [[_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,3,_,8,5],
           [_,_,1,_,2,_,_,_,_],
           [_,_,_,5,_,7,_,_,_],
           [_,_,4,_,_,_,1,_,_],
           [_,9,_,_,_,_,_,_,_],
           [5,_,_,_,_,_,_,7,3],
           [_,_,2,_,1,_,_,_,_],
           [_,_,_,_,4,_,_,_,9]]).

puzzle(2, [[_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_],
           [_,_,1,_,_,_,_,_,_],
           [_,_,_,5,_,7,_,_,_],
           [_,_,4,_,_,_,1,_,_],
           [_,9,_,_,_,_,_,_,_],
           [5,_,_,_,_,_,_,7,3],
           [_,_,2,_,1,_,_,_,_],
           [_,_,_,_,4,_,_,_,9]]).

% Generated printer
chunks(_, [], []).
chunks(N, List, [Chunk|Chunks]) :-
    length(Chunk, N),
    append(Chunk, Rest, List),
    chunks(N, Rest, Chunks).

print_sudoku(Rows) :-
    length(Rows, N),
    B #> 0, N #= B^2, indomain(B),
    W is max(1, ceiling(log10(N+1))),   % digits in the widest number
    maplist(chunks(B), Rows, Chunked),
    chunks(B, Chunked, Bands),
    maplist(print_band(W), Bands).

print_band(W, Band) :-
    maplist(print_row(W), Band),
    nl.

print_row(W, Chunks) :-
    maplist(print_chunk(W), Chunks),
    nl.

print_chunk(W, Cells) :-
    maplist(print_cell(W), Cells),
    write('  ').

print_cell(W, C) :-
    (   var(C) -> Txt = '.' ; Txt = C ),
    format(atom(A), "~w", [Txt]),
    atom_length(A, L),
    Pad is W - L + 1,
    format("~*c~a", [Pad, 0'\s, A]).   % right-align in a column W+1 wide
