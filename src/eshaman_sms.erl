%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_sms).

-export([start_link/1, send/2, send_alarm/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(SRV, ?MODULE).

-record(state, {enabled=false, module=gsms}).

send_alarm(Metric, Value, Alarm, Toggle, ToAddrs) ->
    send(ToAddrs, eshaman_util:format_alarm(Metric, Value, Alarm, Toggle)).

send(ToAddrs, Msg) ->
    gen_server:call(?SRV, {send, ToAddrs, Msg}, infinity).

start_link(Args) ->
    gen_server:start_link({local, ?SRV}, ?MODULE, Args, []).

init(Opts) ->
    Enabled = proplists:get_value(enabled, Opts),
    Module =  proplists:get_value(module, Opts),
    [eshaman_node_adm:register_service(sms, self()) || Enabled],
    {ok, #state{enabled=Enabled, module=Module}}.

handle_call({send, ToAddrs, Msg}, _From, S=#state{enabled=true, module=M}) ->
    {reply, do_send(M, Msg, ToAddrs), S};
handle_call({send, _ToAddrs, _Msg}, _From, S=#state{enabled=false}) ->
    {reply, ok, S}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_send(gsms, Msg, ToAddrs) ->
    IsErrorF =
        fun ({ok, _}) ->
                false;
            (_) ->
                true
        end,
    Errors = [Res ||
                 Res <- [gsms_router:send([{addr, Addr}], Msg) || Addr <- ToAddrs],
                 IsErrorF(Res) == true],
    Res = case Errors of
              [] ->
                  ok;
              [Err|_] ->
                  Err
          end,
    Res;
do_send(Mod, Msg, ToAddrs) ->
    case Mod:send(ToAddrs, Msg) of
        ok ->
            ok;
        {ok, _} ->
            ok;
        Error={error, _} ->
            Error
    end.
