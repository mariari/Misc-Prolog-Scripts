:- module(objvlisp, [class/2, super/2, cname/2]).

dd(nam) :-
    dynamic(name),
    discontiguous(name).

:- dd(class/2).
:- dd(super/2).
:- dd(cname/2).

% Boot strapping process

class(object, class).


super(class, object).
class(class, class).
