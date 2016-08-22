%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Report metrics to master node
%%%
%%% @end
%%% Created : 29 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_proxy_report).

-behaviour(exometer_report).

-include_lib("exometer/include/exometer.hrl").
-include_lib("erlastic_search/include/erlastic_search.hrl").
-include("eshaman.hrl").
-include("log.hrl").

-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
   ]).

-record(st, {proxied_reporter :: atom()
            }).

%%%===================================================================
%%% Probe callbacks
%%%===================================================================

exometer_init(Opts) ->
    sutil:proplist_require([proxied], Opts),
    {ok, #st{proxied_reporter=proplists:get_value(proxied, Opts)}}.

exometer_report(Metric, DataPoint, Extra, Value, #st{proxied_reporter=PR} = St) ->
    Masters = eshaman_node_adm:get_masters(),
    send_report(Metric, DataPoint, Extra, Value, PR, Masters),
    {ok, St}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St}.

exometer_call(Unknown, From, St) ->
    ?info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast({report, Metric, DataPoint, Extra, Value}, St) ->
    exometer_report(Metric, DataPoint, Extra, Value, St);
exometer_cast(Unknown, St) ->
    ?info("Unknown cast: ~p", [Unknown]),
    {ok, St}.

exometer_info(Unknown, St) ->
    ?info("Unknown info: ~p", [Unknown]),
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
send_report(_Metric, _DataPoint, _Extra, _Value, _PR, []) ->
    ok;
send_report(Metric, DataPoint, Extra, Value, PR, [Master|Rest]) ->
    case rpc:call(Master, PR, cast_report, [Metric, DataPoint, Extra, Value]) of
        {badrpc, Reason} ->
            ?error("send report to ~p on ~p ~p", [PR, Master, Reason]),
            send_report(Metric, DataPoint, Extra, Value, PR, Rest);
        _ ->
            ok
    end.
