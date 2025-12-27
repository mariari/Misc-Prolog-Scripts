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
           foldl({Ht, J0, P}/[Ele, I0, I1]>>
                 (I1 is I0 + 1,
                  ht_put(Ht, (I0,J0), Ele))
                , P, 0, _))
         , Puzzle, 0, _).

% This was a ugraph point-rels, but simplified to remove eager comp.
neighboring_points((I,J), ((I,J)-Rels)) :-
    Rels0 = [(I-1,J-1), (I,J-1), (I+1,J-1),
             (I-1,J)  ,          (I+1,J),
             (I-1,J+1), (I,J+1), (I+1,J+1)],
    % Ugly Eval, but w/e
    maplist([(X0,Y0), (X,Y)]>>(X is X0, Y is Y0), Rels0, Rels).

% Looksup the puzzle as an infinite plane
lookup_plane(Table, Index, Value) :-
    (ht_get(Table, Index, Value) -> true; Value = 46). % Hacky default but works

forkable(Table, (Roll-Neighbors)) :-
    maplist(lookup_plane(Table), Neighbors, Numbers),
    maplist(convert, Numbers, Converted),
    lookup_plane(Table, Roll, Code),
    paper(Code),
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
% 1,312,307 inferences, 0.095 CPU in 0.096 seconds (100% CPU, 13776998 Lips)
% 2,429,967 inferences, 0.089 CPU in 0.090 seconds (100% CPU, 27168918 Lips)
% 140,585,748 inferences, 4.440 CPU in 4.451 seconds (100% CPU, 31662188 Lips)
% Time when doing it lazily, after refactor
% % 939,906 inferences, 0.045 CPU in 0.046 seconds (100% CPU, 20718908 Lips)
% % 2,782,767 inferences, 0.096 CPU in 0.096 seconds (100% CPU, 29095856 Lips)
% % 161,753,748 inferences, 5.292 CPU in 5.302 seconds (100% CPU, 30564144 Lips)

% - open('./AOC 2025/day4/test', read, _Str), read_file(_Str, _Input), ht_new(_Ht), time(day4:build_puzzle_table(_Input, _Ht)), ht_get(_Ht, (1,1), Answer).

% ?- open('./AOC 2025/day4/input', read, _Str),
%    read_file(_Str, _Input), ht_new(_Ht),
%    time(day4:build_maze(_Input, _Maze)),
%    day4:forkable_roll(_Maze, Roll).
% Roll = (0, 0)-[(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (…, …)] ;
