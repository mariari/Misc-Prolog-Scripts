:- protocol(set).
   :- public([member/1, members/1, insert/2, union/2, product/2, intersection/2]).
:- end_protocol.

:- object(empty_set, implements(set)).
   empty.
   :- use_module(library(pce), [object/2]).

   insert(X, Set) :- Set = single(X).
   union(Set1, SetUnion) :- implements_protocol(Set1, set), Set1 = SetUnion.
   product(Set, Product) :- Set = Product.
   members([]).
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
   members(Xs) :-
       findall(X, ::member(X), Xs). 
:- end_category.

:- object(single(_S1_), imports(default_set_behaviour), implements(set)).
   member(X) :- _S1_ = X.
:- end_object.

:- object(evens, imports(default_set_behaviour), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X in inf..sup, X mod 2 #= 0.
:- end_object.

:- object(odds, imports(default_set_behaviour), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X in inf..sup, X mod 2 #= 1.
:- end_object.

:- object(union(_S1_, _S2_), imports(default_set_behaviour), implements(set)).
   member(X) :- implements_protocol(_S1_, set), _S1_::member(X).
   member(X) :- implements_protocol(_S2_, set), _S2_::member(X).
:- end_object.

:- object(intersection(_S1_, _S2_), imports(default_set_behaviour), implements(set)).
   member(X) :- implements_protocol(_S1_, set), _S1_::member(X),
                implements_protocol(_S2_, set), _S2_::member(X).
:- end_object.

:- object(product(_S1_, _S2_), imports(default_set_behaviour), implements(set)).
   member((X, Y)) :- implements_protocol(_S1_, set), _S1_::member(X),
                     implements_protocol(_S2_, set), _S2_::member(Y).
:- end_object.

:- object(graph(_V_, _E_)).
   :- public([is_node/1, neighbour/2]).
   is_node(X) :- implements_protocol(_V_, set), _V_::member(X).
   neighbour(X, Neighbour) :-
       implements_protocol(_V_, set), implements_protocol(_E_, set),
       _V_::member(X), _E_::member(Edge),
       implements_protocol(Edge, set),
       Edge::member(X),
       Edge::member(Neighbour),
       X \== Neighbour. 
:- end_object.


%% logtalk_load(lgtunit(loader)), logtalk_load('oo-set', [hook(lgtunit)]).
%% set_tests::run.
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

   test(members) :-
       union(union(single(3), single(5)), single(4))::members(Xs),
       %% Members as list will be ordered DFS. I don't recommend using this as a generator obviously!
       Xs == [3, 5, 4].

   test(unions_membership) :-
       union(single(3), single(5))::member(3).

   test(intersection_membership) :-
       \+ intersection(single(3), single(2))::member(3).

   test(product_membership) :-
       union(single(3), single(2))::product(union(single(4), single(5)), Product),
       combinations([3, 2], [4, 5], Combos),
       forall(lists:member([X, Y], Combos), Product::member((X, Y))).

   test(is_node_is_edge) :-
       Nodes = union(single(a), single(b)),
       %% For an undirected graph, the edges are a set of unordered pairs. AKA, sets!
       Edges = single(union(single(a), single(b))),
       Graph = graph(Nodes, Edges),
       Graph::is_node(a),
       Graph::neighbour(a, b).

%% Note on Unions: if the mem is directly from the second slot, it
%% won't find the simplest solution first due to DFS. We could switch
%% to BFS, since that's more useful for sets.
  %% test(unions_bfs) :-
  %%     union(X, single(5))::member(3),
  %%     X == union(_, single(3)).
:- end_object.
