%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc ICMP ping callback module
%%%
%%% @end
%%% Created : 11 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_icmp).

-behaviour(eshaman_ping).

-export([probe_init/1, probe_ping/2]).

-record(icmp_st, {socket, host}).

%% @doc Init ICMP ping probe instance
%% ++++
%% <p/>
%% ++++
%% _host_ option is required.
%% @end
probe_init(Opts) ->
    sutil:proplist_require([host], Opts),
    Host = proplists:get_value(host, Opts),
    {ok, Socket} = gen_icmp:open(),
    {ok, #icmp_st{host=Host, socket=Socket}}.

probe_ping(Timeout, St=#icmp_st{host=Host, socket=Socket}) ->
    TimeStart = erlang:now(),
    gen_icmp:controlling_process(Socket, self()),
    case gen_icmp:ping(Socket, [Host], [{timeout, Timeout}]) of
        [{ok, Host, _Address, _ReplyAddr, {_Id, _Seq, _TTL, _Elapsed}, _Payload}] ->
            Duration = timer:now_diff(erlang:now(), TimeStart) * 0.001,
            {ok, Duration, St};
        [{error, Error}] ->
            {error, Error, St}
    end.
