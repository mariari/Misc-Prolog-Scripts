%% narwhal.pl — Narwhal mempool DAG layer
%%
%% The DAG lives in the Prolog database as ground facts.
%% Protocol properties are just queries over these facts.
%%
%% Block IDs: b(Validator, Round)
%% Cert  IDs: c(Validator, Round)

:- module(narwhal, [
       % DAG construction
       make_block/5,
       make_cert/4,
       % Validity checks
       valid_block/2,
       valid_cert/2,
       valid_dag/1,
       % Queries
       causal_history/2,
       cert_in_history/2,
       certs_at_round/2,
       blocks_at_round/2,
       quorum/2,
       % Re-export dynamic predicates for bullshark
       block/5,
       cert/5,
       config/1,
       reset_dag/0
   ]).

:- use_module(library(lists), [member/2, subtract/3]).
:- use_module(library(apply)).

%% block(Id, Validator, Round, Txs, CertRefs)
%% cert(Id, BlockId, Validator, Round, Signers)
%% config(config(N, F, Validators))
:- dynamic block/5, cert/5, config/1.

%% reset_dag/0
%  Retract all DAG state.
reset_dag :-
    retractall(block(_, _, _, _, _)),
    retractall(cert(_, _, _, _, _)),
    retractall(config(_)).

%% quorum(+Config, -Q)
%  2f+1 quorum size.
quorum(config(_, F, _), Q) :-
    Q is 2 * F + 1.

%% make_block(+Validator, +Round, +Txs, +CertRefs, -Id)
%  Assert a new block into the DAG.
make_block(Validator, Round, Txs, CertRefs, Id) :-
    Id = b(Validator, Round),
    \+ block(Id, _, _, _, _),
    assertz(block(Id, Validator, Round, Txs, CertRefs)).

%% make_cert(+Validator, +Round, +Signers, -Id)
%  Assert a certificate for the validator's block at this round.
%  Signers must be a set (no duplicate validators).
make_cert(Validator, Round, Signers, Id) :-
    is_set(Signers),
    Id = c(Validator, Round),
    BlockId = b(Validator, Round),
    block(BlockId, _, _, _, _),
    \+ cert(Id, _, _, _, _),
    assertz(cert(Id, BlockId, Validator, Round, Signers)).

%% valid_block(+Id, +Config)
%  A block is valid if:
%  - genesis (round 0): empty cert refs
%  - otherwise: cert refs form a quorum from the previous round
valid_block(b(V, 0), Config) :-
    block(b(V, 0), V, 0, _, CertRefs),
    Config = config(_, _, Validators),
    member(V, Validators),
    CertRefs == [].
valid_block(b(V, R), Config) :-
    R > 0,
    block(b(V, R), V, R, _, CertRefs),
    Config = config(_, _, Validators),
    member(V, Validators),
    PrevR is R - 1,
    % Cert refs must all be from the previous round
    maplist(cert_at_round(PrevR), CertRefs),
    % Must reference a quorum of certs
    length(CertRefs, Len),
    quorum(Config, Q),
    Len >= Q.

cert_at_round(R, c(_, R)).

%% valid_cert(+Id, +Config)
%  A certificate is valid if:
%  - its block exists and is valid
%  - signers form a quorum of distinct validators
valid_cert(Id, Config) :-
    cert(Id, BlockId, _, _, Signers),
    valid_block(BlockId, Config),
    Config = config(_, _, Validators),
    is_set(Signers),
    subset(Signers, Validators),
    length(Signers, Len),
    quorum(Config, Q),
    Len >= Q.

%% valid_dag(+Config)
%  Every block and cert in the DAG is valid.
valid_dag(Config) :-
    forall(block(Id, _, _, _, _), valid_block(Id, Config)),
    forall(cert(Id, _, _, _, _), valid_cert(Id, Config)).

%% causal_history(+CertId, -History)
%  Transitive closure of cert references.
%  Returns a sorted list of cert IDs (deduplicates diamond paths).
causal_history(CertId, History) :-
    causal_history_acc([CertId], [], History).

causal_history_acc([], Acc, History) :-
    sort(Acc, History).
causal_history_acc([C | Rest], Acc, History) :-
    (   member(C, Acc)
    ->  causal_history_acc(Rest, Acc, History)
    ;   cert(C, BlockId, _, _, _),
        block(BlockId, _, _, _, CertRefs),
        append(CertRefs, Rest, NewWork),
        causal_history_acc(NewWork, [C | Acc], History)
    ).

%% cert_in_history(+CertId, +TargetCert)
%  True if TargetCert is in the causal history of CertId.
cert_in_history(CertId, TargetCert) :-
    causal_history(CertId, History),
    member(TargetCert, History).

%% certs_at_round(+Round, -Certs)
certs_at_round(Round, Certs) :-
    findall(Id, cert(Id, _, _, Round, _), Certs).

%% blocks_at_round(+Round, -Blocks)
blocks_at_round(Round, Blocks) :-
    findall(Id, block(Id, _, Round, _, _), Blocks).

%% is_set(+List)
%  True if List contains no duplicates.
is_set(List) :-
    sort(List, Sorted),
    length(List, N),
    length(Sorted, N).

%% subset(+Sub, +Super)
subset([], _).
subset([H | T], Super) :-
    member(H, Super),
    subset(T, Super).
