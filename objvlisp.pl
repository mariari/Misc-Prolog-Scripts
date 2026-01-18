:- module(objvlisp, [class/2, super/2, send/1, has/3]).

dd(nam) :-
    dynamic(name),
    discontiguous(name).

:- dd(class/2).
:- dd(super/2).
:- dd(has/3).

% self is dynamic.
% super is static.

% Representation should be something like
% c. atom.                     For Classes
% 1. class_name(slot1, slot2). For Objects
% 2. class_name{x: 3, y: 4}.   For Objects

% The two object representations have their tradeoffs.
% 1. has the traditional slot accesor offsets.
% 2. is more convinent in prolog due to neseting expressions.
%    Further, it reminds me of CL's make-instance.
% We'll go with 2 for now.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Methods to Implement the Object System %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Method) :-
    Method =.. [Name, Self | Arguments],
    class(Self, SelfClass),
    % Happy and Sad paths
    (lookup(SelfClass, Name, MethodID),
     oapply(MethodID, [Self | Arguments])
    ;
     not(lookup(SelfClass, Name, MethodID)),
     known(SelfClass),
     send(does_not_understand(Self, Method))).


known(Class) :-
    (super(Class, _); Class = object).

lookup(Class, MethodName, MethodID) :-
    has(Class, MethodName, MethodID).

lookup(Class, MethodName, MethodID) :-
    super(Class, Super),
    lookup(Super, MethodName, MethodID).


% Need to union this.
instance_variable(Class, Var).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Done on Records for Objects %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class(Object, Class) :- is_dict(Object, Class).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Boot strapping process. Want a macro %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Behaviour, maybe not needed in bootstrap?
class(behaviour, class).
super(behaviour, object).

% Object
class(object, class).
has(object, initialize, initialize_object).
has(object, does_not_understand, does_not_object).

% Class
class(class, class).
super(class, object).
has(class, initialize, initialize_class).
has(class, new, new_object).
has(class, allocate, allocate_class).

% Some methods initailization


% This is hard as these apply to the dispatch logic in the table.
% Janky table for initialization logic


% This should put data in the instance, but not assertz
class(initialize_object, behaviour).
oapply(initialize_object, [Self, Initialized_Self]).
    % Initialized_Self is Self{},
    % put_dict(class, Self, Name, Initialized_Self).


% This should allocate in the top level and assertz
class(initialize_class, behaviour).
oapply(initialize_class, [Self, Allocated_Self]).

class(allocate_class, behaviour).
oapply(allocate_class, [Self, Allocated_Self]) :-
    Allocated_Self = Self{}.

% Can be removed after we bootstrap proerply
class(does_not_object, behaviour).
oapply(does_not_object, [Self, Arguments]).

% send(foo(object, X, A)).

% A = point{x:1, y:2}, is_dict(A, Tag).
% A = point{x:1, y:2}, is_dict(A, Tag), get_dict(Key, A, Value).

% ?- send(f(object{}, X)).
% ?- send(f(object, X)).
