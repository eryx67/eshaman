%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_xmpp).

-behaviour(gen_server).

-export([start_link/1, send/2, send_alarm/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(SRV, ?MODULE).

-record(state, {xmpp_opts}).

start_link(Args) ->
    application:set_env(escalus, common_test, false),
    gen_server:start_link({local, ?SRV}, ?MODULE, Args, []).

send_alarm(Metric, Value, Alarm, Toggle, ToAddrs) ->
    Msg = eshaman_util:format_alarm(Metric, Value, Alarm, Toggle),
    send(ToAddrs, Msg).

send(ToAddrs, Msg) ->
    gen_server:call(?SRV, {deliver, ToAddrs, Msg}).

init(Opts) ->
    Opts1 =
        sutil:proplist_validate(proplists:substitute_aliases([{domain, server}], Opts),
                                [server, port, username, password],
                                [{server, fun sutil:ensure_binary/1},
                                 {port, fun sutil:ensure_integer/1},
                                 {username, fun sutil:ensure_binary/1},
                                 {password, fun sutil:ensure_binary/1}
                                ],
                                [{host, sutil:ensure_binary(proplists:get_value(server, Opts))},
                                 {auth,{escalus_auth,auth_plain}},
                                 {wspath,undefined},
                                 {compression,<<"zlib">>},
                                 {ssl,optional}
                                ]),
    eshaman_node_adm:register_service(xmpp, self()),
    {ok, #state{xmpp_opts=Opts1}}.

handle_call({deliver, ToAddrs, Msg}, _From, State) ->
    {reply, do_deliver(State, ToAddrs, Msg), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_deliver(#state{xmpp_opts=Opts}, ToAddrs, Msg) ->
    Server = proplists:get_value(server, Opts),
    case sutil:'maybe->'(
      Opts,
      [fun escalus_connection:start/1,
       fun ({ok,Conn,_,_}) ->
               try
                   [begin
                        Addr1 = case binary:match(Addr, <<"@">>) of
                                    nomatch ->
                                        <<Addr/binary, "@", Server/binary>>;
                                    _ -> Addr
                                end,
                        ok = escalus_connection:send(Conn, escalus_stanza:chat_to(Addr1, Msg))
                    end
                    || Addr <- ToAddrs]
               catch
                   _:Error ->
                       ?error("deliver message to some of ~p ~p", [ToAddrs, Error])
               end,
               Conn
       end,
       fun escalus_connection:stop/1
      ]) of
        {ok, _} ->
            ok;
        Error={error, _} ->
            Error
    end.
