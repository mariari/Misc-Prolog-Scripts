:- module(first, [factorial/2, maximum/2, maximum/3, sigma/3, as/2, xy/2]).

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
