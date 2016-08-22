%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 13 Oct 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_alarm_action).

-export([parse_action/2]).

%% internal exports
-export([log/5, smtp/5, xmpp/5, sms/5]).

-include("log.hrl").

parse_action(_IsMaster, Url) ->
    Opts = http_uri:scheme_defaults()
        ++ [{scheme_defaults, [{mailto, 25},
                               {defun, -1},
                               {log, -1},
                               {syslog, -1},
                               {xmpp, 5222},
                               {sms, -1}
                              ]}
           ],
    {ok, {Scheme, UserInfo, Host, _Port, _Path, _Query}} = http_uri:parse(Url, Opts),
    case Scheme of
        defun ->
            [Module, Fun] = [list_to_atom(S) || S <- string:tokens(UserInfo, ":")],
            {module, _} = code:ensure_loaded(Module),
            {Module, Fun, []};
        log ->
            Level = list_to_atom(Host),
            {wrap_action(true, ?MODULE, log, [Level]), []};
        mailto ->
            {wrap_action(false, ?MODULE, smtp, [[UserInfo ++ "@" ++ Host]]), []};
        xmpp ->
            {wrap_action(false, ?MODULE, xmpp, [[sutil:ensure_binary(Host)]]), []};
        sms ->
            {wrap_action(false, ?MODULE, sms, [[sutil:ensure_list(Host)]]), []}
    end.

wrap_action(_IsLocal=true, Mod, Fun, Args) ->
    fun (Metric, Value, Alarm, Toggle) ->
            apply(Mod, Fun, [Metric, Value, Alarm, Toggle] ++ Args)
    end;
wrap_action(false, Mod, Svc, Args) ->
    fun (Metric, Value, Alarm, Toggle) ->
            Masters = eshaman_node_adm:whereis_service(Svc),
            action_proxy(Mod, Svc, [Metric, Value, Alarm, Toggle] ++ Args, Masters)
    end.

log(Metric, Value, Alarm, Toggle, Level) ->
    eshaman_alarm_report:log_alarm(Metric, Value, Alarm, Toggle, Level).

smtp(Metric, Value, Alarm, Toggle, Addr) ->
    eshaman_mail:send_alarm(Metric, Value, Alarm, Toggle, Addr).

xmpp(Metric, Value, Alarm, Toggle, Addr) ->
    eshaman_xmpp:send_alarm(Metric, Value, Alarm, Toggle, Addr).

sms(Metric, Value, Alarm, Toggle, Addr) ->
    eshaman_sms:send_alarm(Metric, Value, Alarm, Toggle, Addr).

action_proxy(_Mod, _Fun, _Args, []) ->
    ok;
action_proxy(Mod, Fun, Args, [Master|Rest]) ->
    case rpc:call(Master, Mod, Fun, Args) of
        {badrpc, Reason} ->
            ?error("call action ~p:~p on ~p ~p", [Mod, Fun, Master, Reason]),
            action_proxy(Mod, Fun, Args, Rest);
        {error, Error} ->
            ?error("call action ~p:~p on ~p ~p", [Mod, Fun, Master, Error]),
            action_proxy(Mod, Fun, Args, Rest);
        _ ->
            ok
    end.
