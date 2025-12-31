:- module(day5, [solve_day5/3, parse_both/4]).

:- autoload(library(dcg/basics), [integer/3]).

range((R1, R2)) --> integer(R1), "-", integer(R2).

% sequence(range), sequence(integer), fails due to how the newlines work
parse_both([R|Rs], Cands) -->
    range(R), "\n", parse_both(Rs, Cands).
parse_both([], Cands) -->
    "\n", parse_both([], Cands).
parse_both([], [C | CRs]) --> integer(C), "\n", parse_both([], CRs).
parse_both([], []) --> "\n"; [].

solve_p1(Ranges, Cs, Answer) :-
    include(within_range(Ranges), Cs, Left),
    length(Left, Answer).

solve_p2(Ranges, Answer) :-
    combine_range(Ranges, Combined),
    maplist([(L, H), Total]>>(Total is H - L + 1), Combined, LengthList),
    sum_list(LengthList, Answer).

% Any by hand
within_range([(L,H) | R], C) :- (between(L, H, C) -> true; within_range(R, C)).

combine_range(R, CR) :-
    sort(R, Sorted),
    combine_range_(Sorted, CR).

combine_range_([(L1, H1), (L2, H2) | Rs] , CRs) :-
    between(L1, H1, L2), !,              % Ranges are inclusive on both ends.
    M is max(H1, H2),                    % H1 may be greater than H2
    combine_range_([(L1, M) | Rs], CRs). % Try combining again
combine_range_([R1, R2 | Rs], [R1 | CRs]) :-
    combine_range_([R2 | Rs], CRs).
combine_range_([R], [R]).
combine_range_([], []).

% phrase_from_file(parse_both(R,_C), './AOC 2025/day5/input'), solve_day5(_, (R, _C), A).

solve_day5(part1, (R,C), Answer) :- solve_p1(R, C, Answer).
solve_day5(part2, (R,_C), Answer) :- solve_p2(R, Answer).
