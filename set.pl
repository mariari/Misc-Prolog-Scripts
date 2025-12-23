:- module(set, [new/1, evens/2, odds/2, set_member/2, not_in_set/2, set_rem/3, set_add/3, set_union/3]).

:- use_module(library(clpfd)).
% :- use_module(library(lambda)).
:- use_module(library(apply)).

:- use_module(library(dif), [dif/2]).

% First version without logtalk... this means I have to lambda it up

all_false(_) :- false.
new(X) :- all_false(X).

evens(Set, X) :- X mod 2 #= 0; call(Set, X).
odds(Set, X)  :- X mod 2 #= 1 ; call(Set, X).

set_add(Set, New_Item, X) :-
    New_Item #= X; call(Set, X).
set_rem(Set, Item_To_Remove, X) :-
    dif(X, Item_To_Remove),
    call(Set, X).
set_union(Set1, Set2, X)  :-
    call(Set1, X);
    call(Set2, X).

set_member(Set, X) :- call(Set, X).
not_in_set(Set, X) :- call(Set, Y), dif(X, Y).

?- set_member(evens(set_rem(set_union(evens(new), odds(new)), 5)), 4).
