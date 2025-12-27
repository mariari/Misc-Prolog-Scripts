:- module(day3, [solve_day3/3]).

max_with_room([H|Ts], N_Room, Max_With_Room) :-
    N_Room #>= 1,
    max_with_room_(Ts, N_Room, (H, Ts), Max_With_Room).

max_with_room_([], _N, Max_Pair, Max_Pair).
max_with_room_([H|T], N, (Best_Prev, Rest_Prev), Max_Left) :-
    (   length(T, Len), Len #>= N - 1,
        H > Best_Prev
    ->  Best_So_Far = (H, T)
    ;   Best_So_Far = (Best_Prev, Rest_Prev)),
    max_with_room_(T, N, Best_So_Far, Max_Left).

max_left(_, 0, []).
max_left(Ls, N, [H|T]) :-
    N #> 0, N_Pred #= N - 1,
    max_with_room(Ls, N, (H, Rest)),
    max_left(Rest, N_Pred, T).

solve_part(N, Input, Answer) :-
    max_left(Input, N, Ans_Codes),
    number_codes(Answer, Ans_Codes).

solve_day3(part1, Input, Answer) :-
    maplist(solve_part(2), Input, Answer_Voltage),
    sum_list(Answer_Voltage, Answer).

solve_day3(part2, Input, Answer) :-
    maplist(solve_part(12), Input, Answer_Voltage),
    sum_list(Answer_Voltage, Answer).

% open('./AOC 2025/day3/input', read, _Str),
% read_file(_Str, _Input),
% solve_day3(_, _Input, Answer).

% X #= 811111111111119, number_codes(X, Cs), day3:max_left(Cs, 12, Answer), number_codes(An, Answer).
