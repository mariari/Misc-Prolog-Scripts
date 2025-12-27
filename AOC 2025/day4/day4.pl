:- module(day4, [solve_day4/3, build_maze/2]).

% Project a vector space into a ugraph like relation.

convert(64, 1). % @
convert(46, 0). % .
paper(64).

% Could turn this into a fact to query but it isn't worth it.
build_maze(Puzzle, (Ht, KeysRel)) :-
    ht_new(Ht), build_puzzle_table(Puzzle, Ht), ht_keys(Ht, Keys),
    maplist(neighboring_points, Keys, KeysRel).

% Kinda an ugly way to grow this
build_puzzle_table(Puzzle, Ht) :-
    foldl({Ht}/[P, J0, J1]>>
          (J1 is J0 + 1,
           foldl({Ht, J0}/[Ele, I0, I1]>>
                 (I1 is I0 + 1,
                  ht_put(Ht, (I0,J0), Ele))
                , P, 0, _))
         , Puzzle, 0, _).

% This was a ugraph point-rels, but simplified to remove eager comp.
neighboring_points((I,J), ((I,J)-Rels)) :-
    I1 is I-1, I2 is I+1,
    J1 is J-1, J2 is J+1,
    Rels = [(I1,J1), (I ,J1), (I2,J1),
            (I1,J ),          (I2,J ),
            (I1,J2), (I ,J2), (I2,J2)].

% Looksup the puzzle as an infinite plane
lookup_plane(Table, Index, Value) :-
    (ht_get(Table, Index, Value) -> true; Value = 46). % Hacky default but works

forkable(Table, (Roll-Neighbors)) :-
    lookup_plane(Table, Roll, Code),
    paper(Code),
    maplist(lookup_plane(Table), Neighbors, Numbers),
    maplist(convert, Numbers, Converted),
    sum_list(Converted, Sum), Sum < 4.

forkable_roll((Table, Keys), Roll) :-
    member(Roll, Keys), forkable(Table, Roll).

all_forkable_rolls(Maze, Forked, Length) :-
    findall(R, forkable_roll(Maze, R), Forked), length(Forked, Length).

% We can make this faster, by scanning the frontier, I.E. the
% neighbors of the removed nodes
remove_until_stable(Maze, Total) :-
    all_forkable_rolls(Maze, Forked, N),
    ( N = 0
    -> Total = 0
    ; remove_forked(Maze, Forked),
      remove_until_stable(Maze, More),
      Total is N + More
    ).

remove_forked((Table, _), Forked) :-
    maplist({Table}/[(Ele-_)]>>(ht_put(Table, Ele, 46)), Forked).

solve_day4(part1, Maze, Answer) :- all_forkable_rolls(Maze, _, Answer).
solve_day4(part2, Maze, Answer) :- remove_until_stable(Maze, Answer).

% ?- open('./AOC 2025/day4/input', read, _Str),
%    read_file(_Str, _Input), ht_new(_Ht),
%    time(build_maze(_Input, _Maze)),
%    time(solve_day4(_, _Maze, Res)).
% Res = 1578 ;
% Res = 10132 .
% Time when using ugraph eagerly
% 1,312,307 inferences, 0.110 CPU in 0.111 seconds (100% CPU, 11912071 Lips)
% 1,699,060 inferences, 0.057 CPU in 0.057 seconds (100% CPU, 29868459 Lips)
% 41,516,665 inferences, 1.319 CPU in 1.321 seconds (100% CPU, 31484434 Lips)
% Time when doing it lazily
% 939,906 inferences, 0.054 CPU in 0.054 seconds (100% CPU, 17494950 Lips)
% 1,926,130 inferences, 0.068 CPU in 0.068 seconds (100% CPU, 28218640 Lips)
% 45,658,555 inferences, 1.479 CPU in 1.483 seconds (100% CPU, 30872258 Lips)
% Speedup from just reordering


% - open('./AOC 2025/day4/test', read, _Str), read_file(_Str, _Input), ht_new(_Ht), time(day4:build_puzzle_table(_Input, _Ht)), ht_get(_Ht, (1,1), Answer).

% ?- open('./AOC 2025/day4/input', read, _Str),
%    read_file(_Str, _Input), ht_new(_Ht),
%    time(day4:build_maze(_Input, _Maze)),
%    day4:forkable_roll(_Maze, Roll).
% Roll = (0, 0)-[(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (…, …)] ;
