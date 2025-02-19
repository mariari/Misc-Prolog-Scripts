% This file is to avoid using modules in vndb.lgt If we don't we run
% into issues where we have to define out the meta_predicates as the
% logtalk compiler doesn't understand the predicates given in the ssl
% library.
:- module(vndb_raw, [fetch_vndb_stats/1, fetch_vndb_stats_tcp/1]).
:- use_module(library(socket)).
:- use_module(library(ssl)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).

% This can be included in the vndb.lgt with the following
% :- meta_predicate(http_open:http_open(*, 0, *)).
% Else it will fail to load
fetch_vndb_stats(JSON) :-
    setup_call_cleanup(
        http_open('https://api.vndb.org/kana/stats', Stream, []),
        json_read_dict(Stream, JSON),
        close(Stream)).

% Modified generated TCP socket code
fetch_vndb_stats_tcp(Response) :-
    Host = 'api.vndb.org',
    % Port = 443,

    % Create and connect a TCP socket
    tcp_socket(Socket),
    tcp_connect(Socket, Host:https),
    tcp_open_socket(Socket, ReadStream, WriteStream),

    % Create SSL context and wrap the streams in SSL
    ssl_context(client, SSLContext, [host(Host)]),
    ssl_negotiate(SSLContext, ReadStream, WriteStream, SSLRead, SSLWrite),

    % Send HTTP request
    format(SSLWrite, 'GET /kana/stats HTTP/1.1~n\c
                      HOST: ~w~n\c
                      User-Agent: SWI-Prolog~n\c
                      Connection: close~n~n', [Host]),
    flush_output(SSLWrite),

    % Read the response
    read_stream_to_codes(SSLRead, ResponseCodes),
    close(SSLRead),
    close(SSLWrite),

    % Convert to string
    string_codes(Response, ResponseCodes).
