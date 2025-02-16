:- protocol(set).
   :- public([member/1, empty/0, insert/2, union/2]).
:- end_protocol.

:- object(empty_set, implements(set)).
   empty.
   %% This is not needed false by default
   member(false).
   insert(X, Set) :- Set = single(X).
   union(Set1, SetUnion) :- Set1 = SetUnion.
:- end_object.

:- category(default_set_behaviour).
   insert(X, Set) :-
       self(Self), Set = union(Self, single(X)).
   union(Set1, SetUnion) :-
       self(Self), SetUnion = union(Self, Set1).
:- end_category.

:- object(single(_S1_), imports(default_set_behaviour), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X #= _S1_.
:- end_object.

:- object(evens, imports(default_set_behaviour), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X mod 2 #= 0.
:- end_object.

:- object(odds, imports(default_set_behaviour), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X mod 2 #= 1.
:- end_object.

:- object(union(_S1_, _S2_), implements(set)).
   member(X) :- implements_protocol(_S1_, set), _S1_::member(X).
   member(X) :- implements_protocol(_S2_, set), _S2_::member(X).
:- end_object.
