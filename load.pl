load_all :-
    prolog_load_context(directory, Here),
    directory_file_path(Here, 'AOC 2025', Aoc),
    asserta(file_search_path(aoc, Aoc)),
    asserta(file_search_path(project, Here)),
    [
        project(first),
        project(second),
        project(set),
        project(vndb_raw),
        project('loader.lgt'),
        project('AOC 2025/aoc_common')
    ].

:- initialization(load_all, now).
