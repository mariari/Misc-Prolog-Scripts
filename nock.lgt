%% Nothing about this is an OO design
%% Instead of having a cell and atom under noun
%% We just have a noun namespace we eval from
:- object(noun).
    :- use_module(library(clpfd)).
    :- public([access/3]).

    access(Noun, 1, Term) :- Term = Noun.
    access([Hd | _], 2, Term) :- Term = Hd.
    access([_ | Tl], 3, Term) :- Term = Tl.
    access(Noun, X, Term) :-
        X #> 3,
        X mod 2 #= 0,
        X #= Y * 2,
        access(Noun, Y, SubNoun),
        access(SubNoun, 2, Term).
    access(Noun, X, Term) :-
        X #> 3,
        X mod 2 #= 1,
        X #= Y * 2 + 1,
        access(Noun, Y, SubNoun),
        access(SubNoun, 3, Term).
:- end_object.

:- object(nock_tests, extends(lgtunit)).
    :- use_module(library(lists), [flatten/2, member/2]).
    test(access) :-
        Term = [[4, 10, 22], [12 | 13], 14, 30],
        flatten(Term, Flat),
        findall(Y, (member(Y, Flat), noun::access(Term, Y, Y)), Flat).
        % findall(Y, noun::access([[4, 10, 22], [12 | 13], 14, 30], Y, Y), List),
        % assert all in List is the same as the elements in Term
:- end_object.
