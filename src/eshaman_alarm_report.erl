%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Report exometer alarms to _eshaman_alarm_
%%%
%%% @end
%%% Created : 29 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_alarm_report).



-behaviour(exometer_report).

-include_lib("exometer/include/exometer.hrl").
-include("log.hrl").

-export([log_alarm/5, cast_report/4]).

%% gen_server callbacks
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
-record(st, {}).

cast_report(Metric, DataPoint, Extra, Value) ->
    exometer_report:cast_reporter(?MODULE, {report, Metric, DataPoint, Extra, Value}).

%%%===================================================================
%%% Probe callbacks
%%%===================================================================

exometer_init([]) ->
    {ok, #st{}}.

exometer_report(Metric, DataPoint, _Extra, Value, #st{} = St) ->
    Key = ets_key(Metric, DataPoint),
    Name = name(Metric, DataPoint),
    ?debug("Report metric ~p = ~p~n", [Name, Value]),
    eshaman_alarm:metric_value(Key, value(Value)),
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

ets_key(Metric, DataPoint) -> Metric ++ [ DataPoint ].

name(Metric, DataPoint) ->
    eshaman_util:metric_name(ets_key(Metric, DataPoint)).

value(V) when is_integer(V) -> V;
value(V) when is_float(V)   -> V;
value(V) when is_boolean(V) -> V;
value(_) -> 0.

log_alarm(Metric, Value, Alarm, Toggle, LogLevel) ->
    Name = name(Metric, ""),
    case LogLevel of
        debug ->
            ?debug("alarm ~p on metric ~p, value ~p ~p~n", [Alarm, Name, Value, Toggle]);
        notice ->
            ?notice("alarm ~p on metric ~p, value ~p ~p~n", [Alarm, Name, Value, Toggle]);
        info ->
            ?info("alarm ~p on metric ~p, value ~p ~p~n", [Alarm, Name, Value, Toggle]);
        error ->
            ?error("alarm ~p on metric ~p, value ~p ~p~n", [Alarm, Name, Value, Toggle]);
        critical ->
            ?critical("alarm ~p on metric ~p, value ~p ~p~n", [Alarm, Name, Value, Toggle]);
        alert ->
            ?alert("alarm ~p on metric ~p, value ~p ~p~n", [Alarm, Name, Value, Toggle]);
        emergency ->
            ?emergency("alarm ~p on metric ~p, value ~p ~p~n", [Alarm, Name, Value, Toggle])

    end.

