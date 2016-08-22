%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Common types
%%%
%%% @end
%%% Created : 11 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>
-module(eshaman_types).

-export_type([metric_name/0, alarm_guard/0, alarm_action/0,
              metric_alarm/0, metric_pattern/0, metric_value/0,
              threshold/0, ttl/0]).
-include("../include/eshaman.hrl").

-type seconds() :: pos_integer().

-type metric_name() :: exometer:name().

-type metric_pattern() :: ['_' | atom()] | '_'.

-type ttl() :: seconds().

-type metric_value() :: number() | boolean().
-type alarm_action() :: fun((metric_name(), metric_value(), alarm_name(),  on|off) -> ok).

-type alarm_guard() ::
      {high, high_alarm()}
    | {low, low_alarm()}
    | {switch, switch_alarm()}.

-type high_alarm() :: #high_alarm{}.

-type low_alarm() :: #low_alarm{}.

-type switch_alarm() :: #switch_alarm{}.

-type alarm_rule() :: #alarm_rule{}.

-type metric_alarm() :: #metric_alarm{}.

-type threshold() :: float().
%% гистерезис для числового уровня события, чтобы произошел сброс
%% события контролируемое значение должно опуститься ниже
%% `AlarmLevel * (1 - AlarmThreshold)`

-type alarm_name() :: high | high2 | low | low2 | switch.
