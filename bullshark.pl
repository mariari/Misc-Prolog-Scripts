%% bullshark.pl — Bullshark consensus ordering layer
%%
%% 2-round waves on top of the Narwhal DAG.
%% Odd rounds = anchor proposal, even rounds = voting.
%%
%% Wave W:
%%   anchor round = 2*W + 1
%%   vote   round = 2*W + 2

:- use_module(narwhal).
:- use_module(library(lists), [member/2]).
:- use_module(library(apply)).

%% wave(+Round, -Wave)
%  Which wave a round belongs to.
wave(Round, Wave) :-
    Round > 0,
    Wave is (Round - 1) // 2.

%% anchor_round(+Wave, -Round)
anchor_round(Wave, Round) :-
    Round is 2 * Wave + 1.

%% vote_round(+Wave, -Round)
vote_round(Wave, Round) :-
    Round is 2 * Wave + 2.

%% leader(+Wave, -Leader)
%  Deterministic leader election: rotate through validators.
leader(Wave, Leader) :-
    config(config(N, _, Validators)),
    Idx is Wave mod N,
    nth0(Idx, Validators, Leader).

%% anchor_cert(?Wave, ?CertId)
%  The anchor certificate for a wave: the leader's cert at the anchor round.
%  Works generatively by enumerating cert facts when Wave is unbound.
anchor_cert(Wave, CertId) :-
    cert(CertId, _, Leader, Round, _),
    Round > 0,
    wave(Round, Wave),
    anchor_round(Wave, Round),
    leader(Wave, Leader).

%% references_cert(+CertId, +TargetCert)
%  True if CertId transitively references TargetCert through the DAG.
%  Uses causal history (transitive reachability), not just direct cert refs.
references_cert(CertId, TargetCert) :-
    cert_in_history(CertId, TargetCert).

%% committed_anchor(+AnchorCert, +Config)
%  Direct commit: the anchor has >= f+1 vote-round certs referencing it.
%  Note: commit threshold is f+1, NOT 2f+1.
committed_anchor(AnchorCert, Config) :-
    Config = config(_, F, _),
    AnchorCert = c(_, AnchorRound),
    cert(AnchorCert, _, _, AnchorRound, _),
    wave(AnchorRound, Wave),
    anchor_round(Wave, AnchorRound),    % verify it's actually an anchor round
    vote_round(Wave, VoteRound),
    Threshold is F + 1,
    % Find vote-round certs that reference this anchor
    findall(VC, (
        cert(VC, _, _, VoteRound, _),
        references_cert(VC, AnchorCert)
    ), VoteCerts),
    length(VoteCerts, Count),
    Count >= Threshold.

%% committed(+CertId, +Config)
%  A cert is committed if:
%  1. It is a directly committed anchor, or
%  2. It is an uncommitted anchor in the causal history of a committed anchor
committed(CertId, Config) :-
    committed_anchor(CertId, Config).
committed(CertId, Config) :-
    % CertId must itself be an anchor
    CertId = c(_, R),
    cert(CertId, _, _, R, _),
    wave(R, W),
    anchor_round(W, R),
    % Find a directly committed anchor at a later wave whose
    % causal history contains CertId
    anchor_cert(CW, CommittedAnchor),
    CW > W,
    committed_anchor(CommittedAnchor, Config),
    causal_history(CommittedAnchor, History),
    member(CertId, History).

%% all_committed_anchors(+Config, -Anchors)
%  All anchors that are committed (directly or transitively), sorted by wave.
all_committed_anchors(Config, Anchors) :-
    findall(W-AC, (
        anchor_cert(W, AC),
        committed(AC, Config)
    ), Pairs),
    sort(Pairs, Sorted),
    pairs_values(Sorted, Anchors).

%% blocks_in_cert_history(+CertId, -Blocks)
%  All block IDs reachable from a cert's causal history.
blocks_in_cert_history(CertId, Blocks) :-
    causal_history(CertId, Certs),
    findall(BlockId, (
        member(C, Certs),
        cert(C, BlockId, _, _, _)
    ), BlockList),
    sort(BlockList, Blocks).

%% total_order(+Config, -Order)
%  Deterministic total ordering of all committed blocks.
%  Committed anchors sorted by wave, then each anchor's new blocks
%  sorted by (Round, Validator).
total_order(Config, Order) :-
    all_committed_anchors(Config, Anchors),
    total_order_acc(Anchors, [], Order).

total_order_acc([], _, []).
total_order_acc([Anchor | Rest], Seen, Order) :-
    blocks_in_cert_history(Anchor, AllBlocks),
    % Only include blocks not yet seen (new to this anchor's batch)
    subtract(AllBlocks, Seen, NewBlocks),
    % Sort by (Round, Validator) for determinism
    sort(NewBlocks, SortedNew),
    append(Seen, SortedNew, NewSeen),
    total_order_acc(Rest, NewSeen, RestOrder),
    append(SortedNew, RestOrder, Order).

%% ============================================================
%% Examples — composable, each builds on the previous
%% ============================================================
%%
%% Usage in REPL:
%%   ?- ex_genesis.                        % base case
%%   ?- ex_wave0.                          % first commit
%%   ?- ex_wave1_skipped.                  % skipped leader
%%   ?- ex_wave2_transitive.              % transitive commit
%%
%% Each calls the previous, so you can start at any point.

%% 1. Genesis — the base case.
%%    4 validators, each with a block and cert at round 0.
%%    Blocks have no cert refs (no prior round to reference).
ex_genesis :-
    reset_dag,
    assertz(config(config(4, 1, [v1, v2, v3, v4]))),
    maplist(genesis_validator, [v1, v2, v3, v4]).

genesis_validator(V) :-
    make_block(V, 0, [], [], _),
    make_cert(V, 0, [v1, v2, v3], _).

%% ?- ex_genesis, config(C), valid_dag(C).
%% true.
%% ?- ex_genesis, blocks_at_round(0, Bs), length(Bs, 4).
%% true.

%% 2. Wave 0 — first complete wave, anchor gets committed.
%%    Round 1 (anchor): leader is v1 (0 mod 4 = 0).
%%    Round 2 (vote): all 4 vote certs reference c(v1,1) — well above f+1=2.
ex_wave0 :-
    ex_genesis,
    R0 = [c(v1,0), c(v2,0), c(v3,0)],
    maplist(round1_block(R0), [v1, v2, v3, v4]),
    maplist(round1_cert, [v1-[v1,v2,v3], v2-[v1,v2,v3], v3-[v1,v2,v3], v4-[v1,v2,v4]]),
    round2_blocks,
    maplist(round2_cert, [v1-[v1,v2,v3], v2-[v1,v2,v3], v3-[v2,v3,v4], v4-[v1,v3,v4]]).

round1_block(R0, V) :- make_block(V, 1, [], R0, _).
round1_cert(V-Signers) :- make_cert(V, 1, Signers, _).

round2_blocks :-
    make_block(v1, 2, [], [c(v1,1), c(v2,1), c(v3,1)], _),
    make_block(v2, 2, [], [c(v1,1), c(v2,1), c(v3,1)], _),
    make_block(v3, 2, [], [c(v1,1), c(v2,1), c(v3,1)], _),
    make_block(v4, 2, [], [c(v1,1), c(v3,1), c(v4,1)], _).
round2_cert(V-Signers) :- make_cert(V, 2, Signers, _).

%% ?- ex_wave0, config(C), committed_anchor(c(v1,1), C).
%% true.
%% ?- ex_wave0, leader(0, L).
%% L = v1.

%% 3. Wave 1 — skipped leader.
%%    Round 3 (anchor): leader is v2 (1 mod 4 = 1).
%%    Round 4 (vote): only v1 references c(v2,3).
%%    1 vote < f+1=2, so NOT directly committed.
ex_wave1_skipped :-
    ex_wave0,
    R2 = [c(v1,2), c(v2,2), c(v3,2)],
    maplist(round3_block(R2), [v1, v2, v3]),
    make_block(v4, 3, [], [c(v2,2), c(v3,2), c(v4,2)], _),
    maplist(round3_cert, [v1-[v1,v2,v3], v2-[v1,v2,v3], v3-[v2,v3,v4], v4-[v1,v3,v4]]),
    % v1 references anchor c(v2,3); v2,v3,v4 skip it
    make_block(v1, 4, [], [c(v1,3), c(v2,3), c(v3,3)], _),
    make_block(v2, 4, [], [c(v1,3), c(v3,3), c(v4,3)], _),
    make_block(v3, 4, [], [c(v1,3), c(v3,3), c(v4,3)], _),
    make_block(v4, 4, [], [c(v1,3), c(v3,3), c(v4,3)], _),
    maplist(round4_cert, [v1-[v1,v2,v3], v2-[v1,v2,v4], v3-[v2,v3,v4], v4-[v1,v3,v4]]).

round3_block(R2, V) :- make_block(V, 3, [], R2, _).
round3_cert(V-Signers) :- make_cert(V, 3, Signers, _).
round4_cert(V-Signers) :- make_cert(V, 4, Signers, _).

%% ?- ex_wave1_skipped, config(C), \+ committed_anchor(c(v2,3), C).
%% true.     % anchor NOT directly committed — only 1 vote
%% ?- ex_wave1_skipped, config(C), \+ committed(c(v2,3), C).
%% true.     % not transitively committed yet either — no later anchor

%% 4. Wave 2 — transitive commit.
%%    Round 5 (anchor): leader is v3 (2 mod 4 = 2).
%%    Round 6 (vote): 3 vote certs reference c(v3,5) — committed.
%%    c(v3,5)'s causal history includes c(v2,3) from wave 1,
%%    so the skipped anchor gets transitively committed.
ex_wave2_transitive :-
    ex_wave1_skipped,
    R4 = [c(v1,4), c(v2,4), c(v3,4)],
    maplist(round5_block(R4), [v1, v2, v3]),
    make_block(v4, 5, [], [c(v2,4), c(v3,4), c(v4,4)], _),
    maplist(round5_cert, [v1-[v1,v2,v3], v2-[v1,v2,v3], v3-[v1,v3,v4], v4-[v2,v3,v4]]),
    R5 = [c(v1,5), c(v2,5), c(v3,5)],
    maplist(round6_block(R5), [v1, v2, v3]),
    make_block(v4, 6, [], [c(v2,5), c(v3,5), c(v4,5)], _),
    maplist(round6_cert, [v1-[v1,v2,v3], v2-[v1,v2,v3], v3-[v2,v3,v4], v4-[v1,v3,v4]]).

round5_block(R4, V) :- make_block(V, 5, [], R4, _).
round5_cert(V-Signers) :- make_cert(V, 5, Signers, _).
round6_block(R5, V) :- make_block(V, 6, [], R5, _).
round6_cert(V-Signers) :- make_cert(V, 6, Signers, _).

%% ?- ex_wave2_transitive, config(C), committed_anchor(c(v3,5), C).
%% true.     % wave 2 anchor directly committed
%% ?- ex_wave2_transitive, config(C), committed(c(v2,3), C).
%% true.     % wave 1 skipped anchor now transitively committed
%% ?- ex_wave2_transitive, config(C), total_order(C, Order), length(Order, N).
%% N = 19.   % deterministic total ordering
