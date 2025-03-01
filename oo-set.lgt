:- protocol(set).
   :- public([member/1, empty/0, insert/2, union/2, product/2, intersection/2, member/2]).
:- end_protocol.

:- object(empty_set, implements(set)).
   empty.
   :- use_module(library(pce), [object/2]).
%% This is not needed false by default
   member(false).
   insert(X, Set) :- Set = single(X).
   union(Set1, SetUnion) :- implements_protocol(Set1, set), Set1 = SetUnion.
   product(Set, Product) :- Set = Product.
:- end_object.

:- category(default_set_behaviour).
   insert(X, Set) :-
       self(Self), Set = union(Self, single(X)).
   union(Set1, SetUnion) :-
       self(Self), SetUnion = union(Self, Set1).
   intersection(Set1, SetIntersection) :-
       self(Self), SetIntersection = intersection(Self, Set1).
   product(Set, SetProduct) :-
    self(Self), SetProduct = product(Self, Set).
:- end_category.

:- object(single(_S1_), imports(default_set_behaviour), implements(set)).
:- use_module(library(clpfd)).
   member(X) :- X #= _S1_.
:- end_object.

:- object(single(_S1_, _S2_), imports(default_set_behaviour), implements(set)).
:- use_module(library(clpfd)).
   member(X, Y) :- X #= _S1_, Y #=_S2_.
:- end_object.

:- object(evens, imports(default_set_behaviour), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X mod 2 #= 0.
:- end_object.

:- object(odds, imports(default_set_behaviour), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X mod 2 #= 1.
:- end_object.

:- object(union(_S1_, _S2_), imports(default_set_behaviour), implements(set)).
   member(X) :- implements_protocol(_S1_, set), _S1_::member(X).
   member(X) :- implements_protocol(_S2_, set), _S2_::member(X).
   member(X, Y) :- implements_protocol(_S1_, set), _S1_::member(X, Y).
   member(X, Y) :- implements_protocol(_S2_, set), _S2_::member(X, Y).
:- end_object.

:- object(intersection(_S1_, _S2_), imports(default_set_behaviour), implements(set)).
   member(X) :- implements_protocol(_S1_, set), _S1_::member(X),
                implements_protocol(_S2_, set), _S2_::member(X).
   member(X, Y) :- implements_protocol(_S1_, set), _S1_::member(X, Y),
                   implements_protocol(_S2_, set), _S2_::member(X, Y).
:- end_object.

:- object(product(_S1_, _S2_), imports(default_set_behaviour), implements(set)).
   member(X, Y) :- implements_protocol(_S1_, set), _S1_::member(X),
                   implements_protocol(_S2_, set), _S2_::member(Y).
:- end_object.


:- object(set_tests, extends(lgtunit)).
   :- public([combinations/3]).
   :- use_module(library(clpfd)).
   :- use_module(library(lists)).

   combinations_aux(_, [], []).
   combinations_aux(A, [B|Bs], [[A, B]|Combos]) :-
       combinations_aux(A, Bs, Combos).

   combinations([], _, []).
   combinations([A|As], Bs, Combos) :-
       combinations_aux(A, Bs, Combos1),
       combinations(As, Bs, Combos2),
       lists:append(Combos1, Combos2, Combos).

   test(nats) :-
       forall(X in 1..100, union(odds, evens)::member(X)).

   test(unions_membership) :-
       union(single(3), single(5))::member(3).

   test(intersection_membership) :-
       \+ intersection(single(3), single(2))::member(3).

   test(product_membership) :-
       union(single(3), single(2))::product(union(single(4), single(5)), Product),
       combinations([3, 2], [4, 5], Combos),
       forall(lists:member([X, Y], Combos), Product::member(X, Y)).

%% Note on Unions: if the mem is directly from the second slot, it
%% won't find the simplest solution first due to DFS. We could switch
%% to BFS, since that's more useful for sets.
  %% test(unions_bfs) :-
  %%     union(X, single(5))::member(3),
  %%     X == union(_, single(3)).
:- end_object.
