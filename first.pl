:- use_module(library(clpfd)).
:- use_module(second).

:- module(first, [colour/1, factorial/2, maximum/2]).
:- autoload(library(lists), [member/2]).

% colour(X) :-
%     X = red;
%     X = green;
%     X = blue.

?- member(X, [a,b,c]).
?- foldl(plus, [0], 0, 1).


colour(red).
colour(green).
colour(blue).

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
