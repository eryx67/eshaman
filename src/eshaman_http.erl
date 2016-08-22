%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc HTTP ping service
%%%
%%% @end
%%% Created : 11 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>
-module(eshaman_http).
-behaviour(eshaman_ping).

-export([request/1, request/3]).

-export([probe_init/1, probe_ping/2]).

-record(http_st, {url, method=get}).

-define(AGENT_STRING, "Mozilla/5.0 (compatible; HttpPing/1.0;").
-define(REQUEST_TIMEOUT, 60000).

%% @doc Init HTTP ping probe instance
%% ++++
%% <p/>
%% ++++
%% _url_ option is required.
%% Other options _{method, get}_
%% @end
probe_init(Opts) ->
    sutil:proplist_require([url], Opts),
    Url = proplists:get_value(url, Opts),
    Method = proplists:get_value(method, Opts, get),
    {ok, #http_st{url=Url, method=Method}}.

probe_ping(Timeout, St=#http_st{url=Url, method=Method}) ->
    TimeStart = erlang:now(),
    case request(Url, Method, Timeout) of
        {ok, {{Code, _ReasonPhrase}, _Headers, _Body}} when Code == 200;
                                                            Code == 301;
                                                            Code == 302;
                                                            Code == 307;
                                                            Code == 308 ->
            Duration = timer:now_diff(erlang:now(), TimeStart) * 0.001,
            {ok, Duration, St};
        {ok, {{Code, _ReasonPhrase}, _Headers, _Body}} ->
            {error, {status_code, Code}, St};
        {error, Error} ->
            {error, Error, St}
    end.

%% @doc Compression (gzip) enabled variant of http:request
%% As http:request/1 in the inets application, but also handles gzip.
%% @end
-type http_response() :: {{integer(), string()}, term(), iolist()}.
-spec request(string()) -> {error, term()} | {ok, http_response()}.
request(URL) ->
    request(URL, get, ?REQUEST_TIMEOUT).

request(URL, Method, Timeout) ->
    case lhttpc:request(URL, Method, [{"User-Agent", ?AGENT_STRING},
                                      {"Host", decode_host(URL)},
                                      {"Accept", "*/*"},
                                      {"Accept-Encoding", "gzip, identity"}],
                        Timeout) of
        {ok, {{StatusCode, ReasonPhrase}, Headers, Body}} ->
            case decode_content_encoding(Headers) of
                identity ->
                    {ok, {{StatusCode, ReasonPhrase}, Headers, Body}};
                gzip ->
                    DecompressedBody = zlib:gunzip(Body),
                    {ok, {{StatusCode, ReasonPhrase}, Headers,
                     DecompressedBody}}
            end;
        E ->
            E
    end.

decode_host(URL) ->
    {_Scheme, _UserInfo, Host, Port, _Path, _Query} =
        eshaman_http_uri:parse(URL),
    case Port of
        80 -> Host;
        N when is_integer(N) ->
            Host ++ ":" ++ integer_to_list(N)
    end.

decode_content_encoding(Headers) ->
    LowerCaseHeaderKeys =
        [{string:to_lower(K), V} || {K, V} <- Headers],
    case lists:keysearch("content-encoding", 1, LowerCaseHeaderKeys) of
        {value, {_, "gzip"}} ->
            gzip;
        {value, {_, "deflate"}} ->
            deflate;
        {value, {_, "identity"}} ->
            identity;
        false ->
            identity
    end.
