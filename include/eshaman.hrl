%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Common types
%%%
%%% @end
%%% Created : 11 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>
-record(high_alarm, {
    level :: number(),
    level2 :: number(),
    threshold = 0.01 :: number() | 'undefined'
}).

-record(low_alarm, {
    level :: number(),
    level2 :: number(),
    threshold = 0.01 :: number() | 'undefined'
}).

-record(switch_alarm, {
    level :: boolean(),
    threshold = undefined
}).

-record(alarm_rule, {
    guard :: eshaman_types:alarm_guard(),
    on_actions = [] :: [eshaman_types:alarm_action()],
    off_actions = []:: [eshaman_types:alarm_action()],
    ttl :: eshaman_types:ttl()
}).

-record(metric_alarm, {
    metrics ::[eshaman_types:metric_pattern()],
    rules :: [eshaman_types:alarm_rule()],
    ttl = 300 :: eshaman_types:ttl() | 'undefined'
}).

-record(rule, {guard :: eshaman_types:switch_alarm()
                      | eshaman_types:high_alarm()
                      | eshaman_types:low_alarm(),
               ttl = 300 :: eshaman_types:ttl(),
               patterns :: [ets:comp_match_spec()],
               on_actions :: [{eshaman_types:alarm_name(), eshaman_types:alarm_action()}],
               off_actions :: [{eshaman_types:alarm_name(), eshaman_types:alarm_action()}]
              }).

-record(elastic_metric, {'@timestamp' :: string(),
                         domain :: string(),
                         group :: string(),
                         name  :: string(),
                         point :: string(),
                         value :: float()
                        }).
