:- module(day4, [solve_day4/3, build_maze/2]).

% Project a vector space into ugraphs An array here would actually
% make it easy, I can do an APL style solution of We have to do that
% but slow withou an equivalent of an Array neighbors([], []).

convert(64, 1). % @
convert(46, 0). % .
paper(64).

neighbor_mapping([P | Uzzle], Neighbors) :-
    length([P | Uzzle], Row),
    length(P, Col),
    Row0 is Row - 1, Col0 is Col - 1,
    all_neighbors((Row0, Col0), Neighbors).

% Could turn this into a fact to query but it isn't worth it.
build_maze_table(Puzzle, Ht) :- ht_new(Ht), build_puzzle_table(Puzzle, Ht).

build_maze(Inp, Maze) :-
    Maze = (Neighbors, Ht),
    neighbor_mapping(Inp, Neighbors),
    build_maze_table(Inp, Ht).

% Kinda an ugly way to grow this
build_puzzle_table(Puzzle, Ht) :-
    foldl({Ht}/[P, J0, J1]>>
          (J1 is J0 + 1,
           foldl({Ht, J0, P}/[Ele, I0, I1]>>
                 (I1 is I0 + 1,
                  ht_put(Ht, (I0,J0), Ele))
                , P, 0, _))
         , Puzzle, 0, _).

% This is all relations assuming infinite space, we basically run the
% applicative ±1 but manually.
neighbors_point((I,J), (I,J)-Rels) :-
    Rels0 = [(I-1,J-1), (I,J-1), (I+1,J-1),
             (I-1,J)  ,          (I+1,J),
             (I-1,J+1), (I,J+1), (I+1,J+1)],
    % Ugly Eval, but w/e
    maplist([(X0,Y0), (X,Y)]>>(X is X0, Y is Y0), Rels0, Rels).

all_neighbors((Len_Row, Len_Col), Graph) :-
    findall(Rel,
            (between(0, Len_Row, Row),
             between(0, Len_Col, Col),
             neighbors_point((Row, Col), Rel)),
            Graph).

% Looksup the puzzle as an infinite plane
lookup_plane(Table, Index, Value) :-
    (ht_get(Table, Index, Value) -> true; Value = 46). % Hacky default but works

forkable(Table, (M-Neighbors)) :-
    maplist(lookup_plane(Table), Neighbors, Numbers),
    maplist(convert, Numbers, Converted),
    lookup_plane(Table, M, Code),
    paper(Code),
    sum_list(Converted, Sum), Sum < 4.

forkable_roll((Mapping, Table), Roll) :-
    member(Roll, Mapping), forkable(Table, Roll).

solve_part1(Maze, Result) :-
    findall(R, forkable_roll(Maze, R), Forked), length(Forked, Result).

% We can make this faster, by scanning the frontier, I.E. the
% neighbors of the removed nodes
remove_until_stable(Maze, Total) :-
    findall(R, forkable_roll(Maze, R), Forked),
    ( Forked == []
    -> Total = 0
    ; length(Forked, N),
      remove_forked(Maze, Forked),
      remove_until_stable(Maze, More),
      Total is N + More
    ).

remove_forked((_Mapping, Table), Forked) :-
    maplist({Table}/[(Ele-_)]>>(ht_put(Table, Ele, 46)), Forked).

solve_day4(part1, Maze, Answer) :- solve_part1(Maze, Answer).
solve_day4(part2, Maze, Answer) :- remove_until_stable(Maze, Answer).

% ?- open('./AOC 2025/day4/input', read, _Str),
%    read_file(_Str, _Input), ht_new(_Ht),
%    time(build_maze(_Input, _Maze)),
%    time(solve_day4(_, _Maze, Res)).
% % 1,135,450 inferences, 0.098 CPU in 0.098 seconds (100% CPU, 11641476 Lips)
% % 2,446,404 inferences, 0.152 CPU in 0.153 seconds (100% CPU, 16050181 Lips)
% Res = 1578 ;
% % 141,741,005 inferences, 4.232 CPU in 4.238 seconds (100% CPU, 33492261 Lips)
% Res = 10132 .


% - open('./AOC 2025/day4/test', read, _Str), read_file(_Str, _Input), ht_new(_Ht), time(day4:build_puzzle_table(_Input, _Ht)), ht_get(_Ht, (1,1), Answer).

% ?- open('./AOC 2025/day4/input', read, _Str),
%    read_file(_Str, _Input), ht_new(_Ht),
%    time(day4:build_maze(_Input, _Maze)),
%    day4:forkable_roll(_Maze, Roll).
% Roll = (0, 0)-[(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (…, …)] ;
