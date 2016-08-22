-module(eshaman_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, eshaman).

-spec start() -> {ok, pid()}.
start() ->
    application:load(?APP),
    sutil:start_app_deps(?APP),
    ok.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    exometer:register_application(),
    Entries = eshaman_env:get(entries, []),
    Alarms = eshaman_env:get(alarms, []),
    Mail = eshaman_env:get(smtp, []),
    XMPP = eshaman_env:get(xmpp, []),
    SMS = eshaman_env:get(sms, []),

    Dist = parse_dist(eshaman_env:get(distribution, [])),
    IsMaster = proplists:get_value(is_master, Dist, true),

    Alarms1 = parse_alarms(IsMaster, Alarms),
    Ret = {ok, _} = eshaman_sup:start_link({IsMaster, Dist, Alarms1, Mail, XMPP, SMS}),

    start_reporters(IsMaster),
    Reporters = [M || {M, _} <- config_reporters(IsMaster)],
    [begin
         exometer:re_register(Key, Metric, MetricOpts),
         [[publish_entry_point(Reporter,
                              Key, Point, Publish, PublishIval, [{publish_name, PublishName}])
           || Point <- Points]
          || Reporter <- Reporters]
     end || {Publish, PublishIval, Points, PublishName,
             {Key, Metric, MetricOpts}} <- parse_entries(Entries)],
    Ret.

stop(_State) ->
    stop_reporters(eshaman_env:get(is_master, true)),
    ok.

start_reporters(IsMaster) ->
    [ok = exometer_report:add_reporter(Mod, Conf)
     || {Mod, Conf} <- config_reporters(IsMaster)].

stop_reporters(IsMaster) ->
    [exometer_report:remove_reporter(Mod) || {Mod, _Conf} <- config_reporters(IsMaster)].

config_reporters(IsMaster) ->
    Elastic = eshaman_env:get(elastic, []),
    Reporters =
        case proplists:get_value(enabled, Elastic) of
            true when IsMaster == true ->
                [{eshaman_elastic_report, Elastic},
                 {eshaman_alarm_report, []}];
            true when IsMaster == false ->
                [{eshaman_elastic_report, [{module, eshaman_proxy_report},
                                           {proxied, eshaman_elastic_report}]},
                 {eshaman_alarm_report, []}];
            false ->
                [{eshaman_alarm_report, []}]
        end,
    Reporters.

publish_entry_point(Reporter, Key, Point, _Publish, PublishIval, Extra) ->
    exometer_report:subscribe(Reporter, Key, Point, PublishIval, Extra, true).

parse_dist(KVs) ->
    sutil:proplist_validate(KVs,
                            [is_master, nodes],
                            [],
                            [{is_master, true},
                             {nodes, []}
                            ]).

parse_alarms(IsMaster, KVs) ->
    [parse_alarm(IsMaster, K, V) || {K, V} <- KVs].

parse_alarm(IsMaster, [Dom, Grp, Name, Metric, Point, Guard], Props) ->
    sutil:proplist_require([level], Props),
    {[Level, TTL, Threshold, OnActions, OffActions], _} =
        sutil:proplist_extract([level, ttl, threshold, on, off], Props),
    [{metrics, [[Dom, Grp, Name, Metric, Point]]},
     {rules, [[{Guard, [{level, Level},
                        {threshold, Threshold}]}
              , {on_actions, [eshaman_alarm_action:parse_action(IsMaster, string:strip(A))
                              || A <- string:tokens(OnActions, ",")]}
              , {off_actions, [eshaman_alarm_action:parse_action(IsMaster, string:strip(A))
                               || A <- string:tokens(OffActions, ",")]}
              , {ttl, TTL}
              ]]
     }].

parse_entries(KVs) ->
    {ok, Defs} = eshaman_env:get(exometer_defaults),
    [begin
         {[Enabled, PublishIval, Points, PublishName], Rest} =
             sutil:proplist_extract(
               [publish, publish_interval, points, publish_name],
               sutil:proplist_validate(Props,
                                       [],
                                       [{publish_interval, fun (V) -> trunc(V * 1000) end},
                                        {interval, fun (V) -> trunc(V * 1000) end},
                                        {span, fun (V) -> trunc(V * 1000) end},
                                        {check_interval, fun (V) -> trunc(V * 1000) end}
                                       ],
                                       [{publish, true}
                                       , {publish_interval, 5000}
                                       , {points, []}
                                       ])),
         {Enabled, PublishIval, Points, PublishName,
          parse_entry(parse_entry_module(K, Defs), K, Rest)}
     end || {K, Props} <- KVs].

parse_entry_module(Key, Defs) ->
    {value, {_, _, Opts}} = lists:keysearch(entry_metric(Key), 2, Defs),
    Module = proplists:get_value(module, Opts),
    Module.

parse_entry(_Module, Key, Props) ->
    {Key, entry_metric(Key), Props}.

entry_metric([_Dom, _Grp, _Name, Metric]) ->
    Metric.
