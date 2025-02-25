:- protocol(set).
   :- public([member/1, empty/0, insert/2, union/2, product/2, member/2]).
:- end_protocol.

:- object(empty_set, implements(set)).
   empty.
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
   interesection(Set1, SetIntersection) :-
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

