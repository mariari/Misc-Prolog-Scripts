%% Nothing about this is an OO design
%% Instead of having a cell and atom under noun
%% We just have a noun namespace we eval from
:- object(noun).
    :- use_module(library(clpfd)).
    :- use_module(library(pce), [object/1, object/2]).
    :- use_module(library(solution_sequences), [distinct/1]).
    :- public([access/3, eval/3, is_cell/2]).

    access(Term, 1, Term).
    access([Term | _], 2, Term).
    access([_ | Term], 3, Term).
    access(Noun, X, Term) :-
        integer(X),
        X #> 3,
        X #= Y * 2,
        access(Noun, Y, SubNoun),
        access(SubNoun, 2, Term).
    access(Noun, X, Term) :-
        integer(X),
        X #> 3,
        X #= Y * 2 + 1,
        access(Noun, Y, SubNoun),
        access(SubNoun, 3, Term).

    is_cell([_|_], 0).
    is_cell(N, 1) :- integer(N).

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
    :- use_module(library(lists), [flatten/2, member/2, append/3]).
    :- use_module(library(clpfd)).
    :- public([index_as_vals/1, middling/1, index_relation/1, index_relation/2]).

    %% TODO make tests "declarative", which I mean, generate the
    %% infinite space of tests such that for any particular example it
    %% is within some generation set

    %% noun::generate(Term), flatten(Term, _Flat), forall(member(A, _Flat), noun::acce

    %% Have index_as_vals() be members of this set for a test!

       % chain(Flat, #<),
    ?- Term = [[_, _ | _], [_ | _], _ | _] ,
       flatten(Term, Flat),
       Flat ins 1..31,
       distinct(Flat),
       label(Flat),
       nock_tests::index_relation(Term).
       % forall(member(A, Flat), noun::access(Term, A, A)).


    index_relation(X) :- integer(X).
    index_relation(X) :- index_relation(X, _).

    index_relation(X, [X]) :- integer(X).
    index_relation([X | XS], Indexs) :-
        index_relation(X, Hds),
        index_relation(XS, Tls),
        ( integer(X) ->
          [Tl | _] = Tls, index_must_be_at_least(X,Tl)
        ; true ),
        append(Hds, Tls, Indexs).

    index_must_be_at_least(X, Z) :-
        X mod 2 #= 0,
        Y #= X + 1,
        Z #>= Y.
    index_must_be_at_least(X,Z) :-
        X mod 2 #= 1,
        Y #= (X + 1) * 2,
        Z #>= Y.

    middling([[4, 10, 22 | 23], [12 | 13], 14, 30 | 31]).
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
