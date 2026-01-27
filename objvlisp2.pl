:- module(objvlisp, [class/2, super/2, send/1, has/3, slots/2]).

dd(Name) :-
    dynamic(Name),
    discontiguous(Name).

:- dd(class/2).
:- dd(super/2).
:- dd(has/3).
:- dd(oapply/2).
:- dd(slots/2).


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

% For method resolution, the default exists to search on the object's class then upwards on the superclasses with no commits except for if at least one method is found
% We override method resolution on the metaclass. It is not currently possible to override it any other way. Perhaps we might want to refine this approach, but overriding method resolution is a core advantage of the system. For example, we can have resolution that commits automatically like objvlisp does naturally, or we could have datalog-style resolution.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Methods to Implement the Object System %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send(Method) :-
    Method =.. [_Name, Self | _Arguments],
    metaclass(Self, MetaClass),
    lookup(MetaClass, send, MethodID),
    oapply(MethodID, [class, Method]).

known(Class) :-
    (super(Class, _); Class = object).

lookup(Class, MethodName, MethodID) :-
    has(Class, MethodName, MethodID).

lookup(Class, MethodName, MethodID) :-
    super(Class, Super),
    lookup(Super, MethodName, MethodID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Done on Records for Objects %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class(Object, Class) :- is_dict(Object, Class).
metaclass(Object, MetaClass) :- class(Object, Class), class(Class, MetaClass).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Boot strapping process. Want a macro %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Behaviour, maybe not needed in bootstrap?
class(behaviour, class).
super(behaviour, object).

% Object
class(object, class).
slots(object, [class]).
has(object, initialize, initialize_object).
has(object, does_not_understand, does_not_object).

% Class
class(class, class).
super(class, object).
has(class, initialize, initialize_class).
has(class, new, new_object).
has(class, allocate, allocate_class).
has(class, send, send_method_default).

% Some methods initailization


% This is hard as these apply to the dispatch logic in the table.
% Janky table for initialization logic


% This should put data in the instance, but not assertz
class(initialize_object, behaviour).
oapply(initialize_object, [Self, _, Self]).


% This should allocate in the top level and assertz
% We should reassert on fact change somehow?!?
class(send_method_default, behaviour).
oapply(send_method_default, [_, Method]) :-
    % Happy and Sad paths
    Method =.. [Name, Self | Arguments],
    class(Self, SelfClass),
    (once(lookup(SelfClass, Name, _))
    -> lookup(SelfClass, Name, MethodID),
       oapply(MethodID, [Self | Arguments])
    ; known(SelfClass),
      send(does_not_understand(Self, Method))).


class(initialize_class, behaviour).
oapply(initialize_class, [Self, _{name: Name, super: O, ivs: Params}, _]) :-
    slots(O, Parent_Slots),
    append(Parent_Slots, Params, AllSlots),
    class(Self, Meta),
    define_class(Name, Meta, AllSlots, O).

% Call retractall and reinstall on changes
define_class(Name, Meta, AllSlots, O) :-
    (\+ super(Name, O)     -> assertz(super(Name, O)) ; true),
    (\+ class(Name, Meta) -> assertz(class(Name, Meta)) ; true),
    % Should we also define the slot accessors?
    (\+ slots(Name, AllSlots) -> assertz(slots(Name, AllSlots)) ; true).


class(allocate_class, behaviour).
oapply(allocate_class, [Self, Allocated_Self]) :-
    Allocated_Self = Self{}.

class(new_object, behaviour).
oapply(new_object,[Self, Arguments, NewSelf]) :-
    send(allocate(Self, Allocated_Self)),
    send(initialize(Allocated_Self, Arguments, NewSelf)).

% can be removed after we bootstrap proerply
class(does_not_object, behaviour).
oapply(does_not_object, _).


% Our new Object definition

:- send(new(class, _{name: point, super: object, ivs: [x, y]}, _)).

has(point, initialize, initial_point).
oapply(initial_point, [AllocSelf, _{x: X, y: Y}, InitSelf]) :-
    InitSelf = AllocSelf.put(x, X).put(y, Y).


% Now for overriding search
%% :- trace(oapply/2).
:- send(new(class, _{name: smalltalk_class, super: object, ivs: []}, _)).

has(smalltalk_class, initialize, initialize_class).
has(smalltalk_class, new, new_object).
has(smalltalk_class, allocate, allocate_class).

has(smalltalk_class, send, smalltalk_send_method).
oapply(smalltalk_send_method, [_, Method]) :-
    % Works like before but commit to one branch
    Method =.. [Name, Self | Arguments],
    class(Self, SelfClass),
    
    (lookup(SelfClass, Name, MethodID) ->
         oapply(MethodID, [Self | Arguments])
    ; known(SelfClass),
      send(does_not_understand(Self, Method))).

:- send(new(smalltalk_class, _{name: smalltalk_point, super: object, ivs: [x, y]}, _)).

has(smalltalk_point, initialize, initial_point).

% ?- send(new(point, #{x: 3, y: 4}, X)).

% ?- send(new(smalltalk_point, #{x: 3, y: 4}, X)).


% ?- class(X, point).

% ?- class(X, Y).

% ?- slots(point, Y), class(point, Z).

% ?- send(new(object, _, A)).
% ?- send(new(class, #{name: point, super: object, ivs: [x, y]}, _)).

% ?- send(new()).

% ?- A = point{x:1, y:2}, is_dict(A, Tag).
% ?- A = point{x:1, y:2}, is_dict(A, Tag), get_dict(Key, A, Value).

% ?- send(f(object{}, X)).
% ?- send(f(object, X)).
