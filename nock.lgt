%% Nothing about this is an OO design
%% Instead of having a cell and atom under noun
%% We just have a noun namespace we eval from
:- object(noun).
    :- use_module(library(clpfd)).
    :- public([access/3, eval/3, is_cell/2]).

    access(Term, 1, Term).
    access([Term | _], 2, Term).
    access([_ | Term], 3, Term).
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

    is_cell([_ | _], 0) :- !.
    is_cell(_, 1).

    % Seems we don't need ::eval here for some reason
    % I'm fine as we aren't parameterized
    eval(Subject, [0 | Axis], Evaled) :-
        access(Subject, Axis, Evaled).
    eval(_Subject, [1 | Value], Value).
    eval(Subject, [2, Subject_Formula | Formula_Formula], Evaled) :-
        eval(Subject, Formula_Formula, FEval),
        eval(Subject, Subject_Formula, SEval),
        eval(SEval, FEval, Evaled).
    eval(Subject, [3 | Sub_Formula], Evaled) :-
        eval(Subject, Sub_Formula, Form),
        is_cell(Form, Evaled).
    eval(Subject, [4 | Sub_Formula], Evaled) :-
        eval(Subject, Sub_Formula, Sub_Eval),
        Evaled #= Sub_Eval + 1.
    eval(Subject, [5, Formula1 | Formula2], true) :-
        eval(Subject, Formula1, Evaled),
        eval(Subject, Formula2, Evaled).
:- end_object.

:- object(nock_tests, extends(lgtunit)).
    :- use_module(library(lists), [flatten/2, member/2]).
    :- public([index_as_vals/1, middling/1]).

    middling([[4, 10, 22], [12 | 13], 14, 30]).
    index_as_vals(X) :- middling(X).
    index_as_vals([2 | 3]).

    test(access_index) :-
        nock_tests::index_as_vals(Term),
        flatten(Term, Flat),
        findall(Y, (member(Y, Flat), noun::access(Term, Y, Y)), Flat).

    test(eval_index) :-
        nock_tests::index_as_vals(Term),
        flatten(Term, Flat),
        \+ (member(A, Flat), noun::eval(Term, [5, [1 | A], 4, 0 | A], false)),
        forall(member(A, Flat), noun::eval(Term, [5, [1 | A], 0 | A], true)).

    test(eval_middling) :-
        nock_tests::middling(X),
        noun::eval(X, [3, 0 | 1], 0),
        noun::eval(X, [3, 0 | 4], 1),
        noun::eval(X, [4, 0 | 4], 5),
        noun::eval(X, [0 | 1], X).
:- end_object.
