:- object(kana).
   :- use_module(project(vndb_raw)).
   :- public([stats/1]).

   stats(JSON) :- fetch_vndb_stats(JSON).
:- end_object.

:- object(kana_tcp).
   :- use_module(project(vndb_raw)).
   :- public([stats/1]).

   stats(Response) :- fetch_vndb_stats_tcp(Response).

:- end_object.
