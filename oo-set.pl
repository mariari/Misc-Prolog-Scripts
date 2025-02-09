:- use_module(library(logtalk)).

:- protocol(set).
   :- public([member/1]).
:- end_protocol.

:- object(empty_set, implements(set)).
   member(_) :- false.
:- end_object.

:- object(evens, implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X mod 2 #= 0.
:- end_object.

:- object(odds, implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X mod 2 #= 1.
:- end_object.

:- object(single(_S1_), implements(set)).
   :- use_module(library(clpfd)).
   member(X) :- X #= _S1_.
:- end_object.

:- object(union(_S1_, _S2_), implements(set)).
   member(X) :- _S1_::member(X).
   member(X) :- _S2_::member(X).
:- end_object.
