%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_util).

-export([metric_name/1, format_alarm/4, format_alarm/5]).

format_alarm(Metric, Value, Alarm, Toggle) ->
    format_alarm(Metric, Value, Alarm, Toggle, "\r\n").

format_alarm(Metric, Value, Alarm, Toggle, Sep) ->
    Name = metric_name(Metric),
    Severity = case Toggle of
                   on -> "ALARM";
                   off -> "NOTIFICATON"
               end,
    Msg =  io_lib:fwrite("WATCHDOG ~s~s~s.~s is ~s, value ~p~s",
                         [Severity, Sep, Name, Alarm, Toggle, Value, Sep]),
    lists:flatten(Msg).

metric_name(Metric) ->
    iolist_to_binary(intersperse(".", lists:map(fun sutil:ensure_list/1, Metric))).

intersperse(_, [])         -> [];
intersperse(_, [X])        -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].
