:- module(logical, [connected/3, nearby/2, reachable/2]).
:- autoload(library(lists), [is_set/1]).

% These should be communititive, so do this, not in the book
connected(X, Y, Z) :- connected_(X, Y, Z).
connected(X, Y, Z) :- connected_(Y, X, Z).

connected_(bond_street,oxford_circus,central).
connected_(oxford_circus,tottenham_court_road,central).
connected_(bond_street,green_park,jubilee).
connected_(green_park,charing_cross,jubilee).
connected_(green_park,piccadilly_circus,piccadilly).
connected_(piccadilly_circus,leicester_square,piccadilly).
connected_(green_park,oxford_circus,victoria).
connected_(oxford_circus,piccadilly_circus,bakerloo).
connected_(piccadilly_circus,charing_cross,bakerloo).
connected_(tottenham_court_road,leicester_square,northern).
connected_(leicester_square,charing_cross,northern).

%% They are nearby if they are on the same line, with at most 1
%% station between them.
nearby(X, Y) :- connected(X, Y, _L).
nearby(X, Y) :- connected(X, Z, L), connected(Z, Y, L).

% This rule sucks, gets repeat results over and over, lets fix it
reachable(X, Y) :- reachable_(X, Y, _).

reachable_(X, Y, _) :- connected(X, Y, _).
reachable_(X, Y, Seen) :-
    connected(X, Z, _),
    NewSeen = [Z | Seen],
    % \+ member(Z, Seen),
    is_set(NewSeen),
    reachable_(Z, Y, NewSeen).
