:- module(first, [factorial/2, maximum/2, maximum/3,
                  sigma/3, as/2, xy/2, qsort/2]).

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
    N #> 0, Res #> 1,
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

