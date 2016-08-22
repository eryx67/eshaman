%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%% Metric notification
%% ++++
%% <p/>
%% ++++
%% .*Уровни логирования событий*
%%
%% high2,low2,switch:: alert
%% high,low:: critical
%% сброс события:: notice
%%
%%% @end
%%% Created : 14 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_alarm).

-behaviour(gen_server).

-compile([export_all]).

-export([start_link/0, start_link/1, state/0, metric_value/2]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -include_lib("stdlib/include/ms_transform.hrl").

-include_lib("sutil/include/sutil.hrl").
-include("../include/eshaman.hrl").
-include("../include/log.hrl").

-define(SRV, ?MODULE).

-define(THRESHOLD, 0.05).
-define(TTL, 300).

-type threshold() :: eshaman_types:threshold().
-type ttl() :: eshaman_types:ttl().
-type alarm_action() :: eshaman_types:alarm_action().
-type alarm_name() :: eshaman_types:alarm_name().
-type metric_name() :: eshaman_types:metric_name().
-type metric_value() :: eshaman_types:metric_value().
-type metric_pattern() :: eshaman_types:metric_pattern().

-type alarm_level() :: number() | undefined.
%% числовой уровень наступления события

-type alarm_pattern_return() :: {alarm_name(), on | off, metric_name(), term()}.

-type metric_guard() :: {high, alarm_level(), alarm_level(), threshold(), ttl()}
                      | {low, alarm_level(), alarm_level(), threshold(), ttl()}
                      | {switch, boolean(), ttl()}.
%% `{high|low, Level, Level2, Threshold, TTL}`::
%% событие имеет два уровня, при выходе за пределы уровня устанавливается
%% событие, при возвращении в пределы происходит сброс события. Для `Level2`
%% также выполняются действия события
%% `{switch, Level}`::
%% логический переключатель, при изменении состояния также выполняются действия
%% события

-type alarm_spec() :: {metric_pattern(), metric_guard(), ttl(), [alarm_action()], [alarm_action()]}.
%% `{Pattern, Guard, ActionsOn, ActionsOff}`::
%%  _ActionsOn_, _ActionsOff_ выполняются при наступлении/сбросе события

-record(state, {rules = [] :: [#rule{}],
                active_alarms=active_alarms_new()}).

start_link() ->
    start_link(eshaman_env:get(alarms, [])).

-spec start_link(AlarmsSpec::[alarm_spec()]) ->
                        sutil:maybe(pid(), term()).
start_link(Alarms) ->
    gen_server:start_link({local, ?SRV}, ?MODULE, {Alarms}, []).

%% debug calls
state() ->
    gen_server:call(?SRV, {state}).

metric_value(MetricName, Value) ->
    gen_server:cast(?SRV, {metric_value, MetricName, Value}).

%% gen_server callbacks
init({Config}) ->
    Alarms = parse_config(Config),
    sutil:maybe(fun () ->
                        Rs = [compile_alarm_spec(AS) || AS <- Alarms],
                        Rs
                end,
                fun (Rules) ->
                        #state{rules=Rules}
                end,
                fun (Error) ->
                        {error, Error}
                end).

handle_call({state}, _From, S=#state{rules=Rs, active_alarms=As}) ->
    {reply, [{rules, Rs}, {active_alarms, active_alarms_list(As)}], S}.

handle_cast({metric_value, MetricName, Value}, S=#state{rules=Rs, active_alarms=As}) ->
    As1 = handle_metric_value(MetricName, Value, Rs, As),
    {noreply, S#state{active_alarms=As1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
-spec parse_config(Config::[#metric_alarm{}]) -> [alarm_spec()].
parse_config(Config) ->
    AlarmSpecs = lists:flatten([alarm_config_to_spec(C) || C <- Config]),
    [?maybe_unlift(validate_alarm_spec(AS)) || AS <- AlarmSpecs].

alarm_config_to_spec(Config=[{_, _}|_]) ->
    RuleKV_F = fun ({high, KVs}) ->
                    {guard, eshaman_recs:'#fromlist-high_alarm'(KVs)};
                ({low, KVs}) ->
                    {guard, eshaman_recs:'#fromlist-low_alarm'(KVs)};
                ({switch, KVs}) ->
                    {guard, eshaman_recs:'#fromlist-switch_alarm'(KVs)};
                (KV) ->
                    KV
            end,
    MA = eshaman_recs:'#fromlist-metric_alarm'(Config),
    #metric_alarm{rules=Rules} = MA,
    Rules1 = [begin
                  eshaman_recs:'#fromlist-alarm_rule'([RuleKV_F(KV) || KV <- R])
              end || R <- Rules],
    alarm_config_to_spec(MA#metric_alarm{rules=Rules1});
alarm_config_to_spec(#metric_alarm{
                        metrics=Metrics,
                        rules=Rules,
                        ttl = TTL
                       }) ->
    ParseRuleF = fun (#alarm_rule{
                         guard=Guard,
                         on_actions=OnActions,
                         off_actions=OffActions,
                         ttl=RuleTTL
                        }, Metric) ->
                         RuleTTL1 = if RuleTTL == undefined -> TTL;
                                       true -> RuleTTL
                                    end,
                         {Metric, Guard, RuleTTL1, OnActions, OffActions}
                 end,
    lists:flatten([[ParseRuleF(R, S) || R <- Rules] || S <- Metrics]).

handle_metric_value(MetricName, Value, Rules, ActiveAlarms) ->
    case rules_find(MetricName, Value, Rules) of
        [] ->
            ActiveAlarms;
        Events ->
            lists:foldl(fun (AE={{_Alarm, _Event, _Val}, _Rule}, AAs) ->
                                process_alarm_event(MetricName, AE, AAs)
                        end, ActiveAlarms, Events)
    end.

process_alarm_event(MetricName, {{Alarm, Event, Value}, Rule=#rule{ttl=TTL}}, ActiveAlarms) ->
    Now = os:timestamp(),
    {LastAlarm, LastTimestamp} = active_alarms_get(MetricName, {undefined, Now}, ActiveAlarms),
    Expired = (timer:now_diff(Now, LastTimestamp) div 1000000) > TTL,
    {NewAlarm, ActionEvents} = process_alarm_event_1(Alarm, LastAlarm, Event),

    [run_alarm_event(ActionAlarm, ActionEvent, MetricName, Value, Rule)
     || {ActionAlarm, ActionEvent} <- ActionEvents,
        ((ActionAlarm =/= LastAlarm) orelse Expired orelse (ActionEvent == off))],

    case {NewAlarm, LastAlarm} of
        {undefined, undefined} ->
            ActiveAlarms;
        {undefined, _} ->
            active_alarms_del(MetricName, ActiveAlarms);
        {LastAlarm, LastAlarm} when not Expired ->
            ActiveAlarms;
        {LastAlarm, LastAlarm} when LastAlarm =/= Alarm ->
            ActiveAlarms;
        _ ->
            active_alarms_set(MetricName, NewAlarm, ActiveAlarms)
    end.

process_alarm_event_1(Alarm, undefined, on) ->
    {Alarm, [{Alarm, on}]};
process_alarm_event_1(high, high2, on) ->
    {high, [{high2, off}, {high, on}]};
process_alarm_event_1(low, low2, on) ->
    {low, [{low2, off}, {low, on}]};
process_alarm_event_1(Alarm, _LastAlarm, on) ->
    {Alarm, [{Alarm, on}]};
process_alarm_event_1(Alarm, Alarm, threshold) ->
    {Alarm, []};
process_alarm_event_1(high2, undefined, threshold) ->
    {high, [{high, on}]};
process_alarm_event_1(low2, undefined, threshold) ->
    {low, [{low, on}]};
process_alarm_event_1(_Alarm, LastAlarm, threshold) ->
    {LastAlarm, []};
process_alarm_event_1(_A, undefined, off) ->
    {undefined, []};
process_alarm_event_1(high, high2, off) ->
    {undefined, [{high2, off}]};
process_alarm_event_1(low, low2, off) ->
    {undefined, [{low2, off}]};
process_alarm_event_1(Alarm, Alarm, off) ->
    {undefined, [{Alarm, off}]};
process_alarm_event_1(_A, LastAlarm, off) ->
    {LastAlarm, []}.

rules_find(MetricName, Val, Rules) ->
    Res = lists:foldl(fun (Rule, Acc) ->
                              case apply_rule(MetricName, Val, Rule) of
                                  undefined ->
                                      Acc;
                                  {Alarm, Event, _} ->
                                      [{{Alarm, Event, Val}, Rule}|Acc]
                              end
                      end, [], Rules),
    %% off alarms must be first
    lists:usort(fun ({{A, E, _}, _}, {{A, E, _}, _}) -> true;
                    ({{A1, E, _}, _}, {{A2, E, _}, _}) -> A1 < A2;
                    ({{_, off, _}, _}, {{_, E, _}, _}) when E =/= off -> true;
                    ({{_, E, _}, _}, {{_, off, _}, _}) when E =/= off -> false
                end, Res).

apply_rule(MetricName, Val, #rule{patterns=Patterns}) ->
    apply_rule_patterns(MetricName, Val, Patterns).

apply_rule_patterns(_MetricName, _Val, []) ->
    undefined;
apply_rule_patterns(MetricName, Val, [P|Ps]) ->
    case apply_alarm_pattern(MetricName, Val, P) of
        undefined ->
            apply_rule_patterns(MetricName, Val, Ps);
        Res ->
            Res
    end.

run_alarm_event(Alarm, Event, MetricName, Value, Rule) ->
    log_alarm_event(Alarm, Event, MetricName, Value),
    run_alarm_actions(Alarm, Event, MetricName, Value, Rule).

log_alarm_event(Alarm, on, MetricName, Value) when Alarm == switch;
                                                 Alarm == high2;
                                                 Alarm == low2 ->
    ?alert("alarm ~p on ~p, value ~p", [Alarm, MetricName, Value]);
log_alarm_event(Alarm, on, MetricName, Value) ->
    ?critical("alarm ~p on ~p, value ~p", [Alarm, MetricName, Value]);
log_alarm_event(Alarm, off, MetricName, Value) ->
    ?notice("alarm ~p off ~p, value ~p", [Alarm, MetricName, Value]).

run_alarm_actions(Alarm, Event, MetricName, Value, #rule{on_actions=OnAs,
                                                        off_actions=OffAs}) ->
    As = case Event of
             on -> OnAs;
             off -> OffAs
         end,
    case [Actions || {AN, Actions} <- As, AN =:= Alarm] of
        [] ->
            ok;
        [Actions] ->
            [run_alarm_action(A, Alarm, Event, MetricName, Value) || A <- Actions]
    end.

run_alarm_action(Action, Alarm, Event, MetricName, Value) ->
    spawn(
      fun () ->
              sutil:maybe(
                fun () ->
                        case Action of
                            {M, F, A} ->
                                apply(M, F, [MetricName, Value, Alarm, Event] ++ A);
                            {F, A} ->
                                apply(F, [MetricName, Value, Alarm, Event] ++ A)
                        end
                end,
                fun (_) -> ok end,
                fun (Error) ->
                        ?error("running event action ~p for ~p, ~p",
                               [Action, {MetricName, Value, Alarm, Event}, Error]),
                        {error, Error}
                end)
      end).

compile_alarm_spec({Pattern, Guard, TTL, OnActions, OffActions}) ->
    Rule = compile_alarm_patterns(Guard, Pattern, OnActions, OffActions),
    Rule#rule{ttl=TTL}.

-spec apply_alarm_pattern(metric_name(), metric_value(), ets:comp_match_spec()) ->
                                 undefined | alarm_pattern_return().
apply_alarm_pattern(MetricName, Val, Pattern) ->
    case ets:match_spec_run([{MetricName, Val}], Pattern) of
        [Ret] ->
            Ret;
        [] ->
            undefined
    end.

compile_alarm_patterns(G=#switch_alarm{level=Lvl1}, Pattern, OnActions, OffActions) ->
    #rule{guard=G,
          patterns=[compile_bool_pattern(Pattern, {switch, on}, Lvl1),
                    compile_bool_pattern(Pattern, {switch, off}, not Lvl1)
                   ],
          on_actions=[{switch, OnActions}],
          off_actions=[{switch, OffActions}]
         };
compile_alarm_patterns(G=#high_alarm{level=Lvl1, level2=undefined},
                       Pattern, OnActions, OffActions) ->
    compile_alarm_patterns(G#high_alarm{level=undefined, level2=Lvl1},
                           Pattern, OnActions, OffActions);
compile_alarm_patterns(G=#high_alarm{level=Lvl1, level2=Lvl2, threshold=Thr},
                       Pattern, OnActions, OffActions) ->
    Patterns =
        [compile_number_pattern(Pattern, {high2, on}, '>', Lvl2),
         compile_number_pattern(Pattern, {high2, threshold}, '>', Lvl2 * (1 - Thr)),
         if Lvl1 == undefined -> undefined;
            true ->compile_number_pattern(Pattern, {high, on}, '>', Lvl1)
         end,
         if Lvl1 == undefined -> undefined;
            true -> compile_number_pattern(Pattern, {high, off}, '<', Lvl1 * (1 - Thr))
         end,
         compile_number_pattern(Pattern, {high2, off}, '<', Lvl2 * (1 - Thr))
        ],
    #rule{guard=G,
          patterns=[P || P <- Patterns, P =/= undefined],
          on_actions=[{high, OnActions}, {high2, OnActions}],
          off_actions=[{high, OffActions}, {high2, OffActions}]
         };
compile_alarm_patterns(G=#low_alarm{level=Lvl1, level2=undefined},
                       Pattern, OnActions, OffActions) ->
    compile_alarm_patterns(G#low_alarm{level=undefined, level2=Lvl1},
                           Pattern, OnActions, OffActions);
compile_alarm_patterns(G=#low_alarm{level=Lvl1, level2=Lvl2, threshold=Thr},
                       Pattern, OnActions, OffActions) ->
    Patterns = [compile_number_pattern(Pattern, {low2, on}, '<', Lvl2),
                compile_number_pattern(Pattern, {low2, threshold}, '<', Lvl2 * (1 + Thr)),
                if Lvl1 == undefined -> undefined;
                   true ->compile_number_pattern(Pattern, {low, on}, '<', Lvl1)
                end,
                if Lvl1 == undefined -> undefined;
                   true -> compile_number_pattern(Pattern, {low, off}, '>', Lvl1 * (1 + Thr))
                end,
                compile_number_pattern(Pattern, {low2, off}, '>', Lvl2 * (1 + Thr))
               ],
    #rule{guard=G,
          patterns=[P || P <- Patterns, P =/= undefined],
          on_actions=[{low, OnActions}, {low2, OnActions}],
          off_actions=[{low, OffActions}, {low2, OffActions}]
         }.

%% @doc
%% возвращает `{switch, on|off, metric_name(), Value}`
%% @end
compile_bool_pattern(MetricPattern, AlarmEvent, Val)
  when Val == true;
       Val == false ->
    compile_guard_pattern(MetricPattern, AlarmEvent, '==', Val).

%% @doc
%% возвращает `{high|high2|low|low2, on|off, MetricName, Value}`
%% @end
compile_number_pattern(MetricPattern, AlarmEvent, GuardF, Val)
  when (GuardF == '>' orelse GuardF == '<')
       andalso is_number(Val) ->
    compile_guard_pattern(MetricPattern, AlarmEvent, GuardF, Val).

compile_guard_pattern(MetricPattern, {Alarm, Event}, GuardF, Val) ->
    ValVar = '$1',
    MH = {MetricPattern, ValVar},
    MC = [{GuardF, ValVar, Val}],
    MB = [{{Alarm, Event, ValVar}}],
    MF = {MH, MC, MB},
    ets:match_spec_compile([MF]).

validate_alarm_spec(Spec={Pattern, Guard, TTL, OnActions, OffActions}) ->
    sutil:maybe(fun () ->
                        Pattern1 = validate_metric_pattern(Pattern),
                        Guard1 = validate_alarm_guard(Guard),
                        OnActions1 = validate_actions(OnActions),
                        OffActions1 = validate_actions(OffActions),
                        TTL1 = validate_ttl(TTL),
                        {Pattern1, Guard1, TTL1, OnActions1, OffActions1}
                end,
                fun (Res) -> Res end,
                fun (Error) ->
                        throw({error, {invalid_alarm, {Spec, Error}}})
                end).

validate_actions(Action) when not is_list(Action) ->
    validate_actions([Action]);
validate_actions(Actions) ->
    [validate_action(A) || A <- Actions].

validate_action(Action={M, F, A})  when is_atom(M),
                                 is_atom(F),
                                 is_list(A) ->
    {module, _} = code:ensure_loaded(M),
    Action;
validate_action(Action={F, A}) when is_function(F, 4 + length(A)) ->
    Action.

validate_metric_pattern(P='_') ->
    P;
validate_metric_pattern(Path) when is_list(Path) ->
    Path.

validate_alarm_guard({switch, Level}) ->
validate_alarm_guard(#switch_alarm{level=Level});
validate_alarm_guard(A=#switch_alarm{level=Level}) ->
    Level1 = validate_switch_value(Level),
    A#switch_alarm{level=Level1};
validate_alarm_guard({Name, Level}) when Name == high;
                                         Name == low ->
    validate_alarm_guard({Name, undefined, Level, ?THRESHOLD});
validate_alarm_guard({high, Level, Level2, Thr}) ->
    validate_alarm_guard(#high_alarm{level=Level, level2=Level2, threshold=Thr});
validate_alarm_guard({low, Level, Level2, Thr}) ->
    validate_alarm_guard(#low_alarm{level=Level, level2=Level2, threshold=Thr});
validate_alarm_guard(A=#high_alarm{level=Level, level2=Level2, threshold=Thr}) ->
    Thr1 = validate_threshold(Thr),
    {L1, L2} = validate_high_levels(Level, Level2, Thr1),
    A#high_alarm{level=L1, level2=L2, threshold=Thr1};
validate_alarm_guard(A=#low_alarm{level=Level, level2=Level2, threshold=Thr}) ->
    Thr1 = validate_threshold(Thr),
    {L1, L2} = validate_low_levels(Level, Level2, Thr1),
    A#low_alarm{level=L1, level2=L2, threshold=Thr1}.

validate_switch_value(V) when V == true;
                              V == false  ->
    V;
validate_switch_value(V) ->
    throw({error, {invalid_value, V}}).

validate_high_levels(undefined, H2, _Thr) when is_number(H2) ->
    {undefined, H2};
validate_high_levels(H, undefined, _Thr) when is_number(H) ->
    {undefined, H};
validate_high_levels(H, H2, Thr) when H2 < H ->
    validate_high_levels(H2, H, Thr);

validate_high_levels(H, H2, Thr) when is_number(H),
                                      is_number(H2) ->
    case max(0, H2 * (1 - Thr) - H) of
        0 ->
            {undefined, H2};
        _ ->
            {H, H2}
    end;
validate_high_levels(H, H2, _) ->
    throw({error, {invalid_levels, {H, H2}}}).


validate_low_levels(undefined, L2, _Thr) when is_number(L2) ->
    {undefined, L2};
validate_low_levels(L, undefined, _Thr) when is_number(L) ->
    {undefined, L};
validate_low_levels(L, L2, Thr) when L2 > L ->
    validate_low_levels(L2, L, Thr);

validate_low_levels(L, L2, Thr) when is_number(L),
                                     is_number(L2) ->
    case max(0, L - L2 * (1 - Thr)) of
        0 ->
            {undefined, L2};
        _ ->
            {L, L2}
    end;
validate_low_levels(L, L2, _) ->
    throw({error, {invalid_levels, {L, L2}}}).

validate_threshold(Thr) when is_float(Thr),
                             Thr > 0,
                             Thr < 1 ->
    Thr;
validate_threshold(undefined) ->
    ?THRESHOLD;
validate_threshold(Thr) ->
    throw({error, {invalid_threshold, Thr}}).

validate_ttl(undefined) ->
    ?TTL;
validate_ttl(TTL) when is_integer(TTL),
                       TTL > 0 ->
    TTL;
validate_ttl(TTL) ->
    throw({error, {invalid_ttl, TTL}}).

active_alarms_new() ->
    dict:new().

active_alarms_get(MetricName, Default, Db) ->
    case dict:find(MetricName, Db) of
        {ok, Res} ->
            Res;
        error ->
            Default
    end.

active_alarms_set(MetricName, Alarm, Db) ->
    dict:store(MetricName, {Alarm, os:timestamp()}, Db).

active_alarms_del(MetricName, Db) ->
    dict:erase(MetricName, Db).

active_alarms_list(Db) ->
    dict:to_list(Db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compile_alarm_spec_test_() ->
    %% {Pattern, Guard, Metrics, TTL, OnActions, OffActions}
    HighAlarm = #high_alarm{level=4, level2=5, threshold=0.1},
    LowAlarm = #low_alarm{level=2, level2=1, threshold=0.1},
    SwitchAlarm = #switch_alarm{level = true},
    MetricPath1 = ['d1', 'd2', 'sensor1'],
    Metric1 = 'out1',
    Metric2 = 'out2',
    MetricName1 = MetricPath1 ++ [Metric1],
    MetricName2 = MetricPath1 ++ [Metric2],
    TTL = 1,
    OnActions = OffActions = [{fun send_alarm_event/4, []}],

    HighSpecs = [{MN, HighAlarm, TTL, OnActions, OffActions} || MN <- [MetricName1, MetricName2]],
    LowSpecs = [{MN, LowAlarm, TTL, OnActions, OffActions} || MN <- [MetricName1, MetricName2]],
    SwitchSpecs = [{MN, SwitchAlarm, TTL, OnActions, OffActions} || MN <- [MetricName1, MetricName2]],

    %% match spec
    MetricPathMatch1 = ['_', 'd2', 'sensor1', 'out1'],
    MetricPathMatch2 = '_',

    HighSpecMatch1 = {MetricPathMatch1, HighAlarm, TTL, OnActions, OffActions},
    HighSpecMatch2 = {MetricPathMatch2, HighAlarm, TTL, OnActions, OffActions},

    HighRules = [compile_alarm_spec(HS) || HS <- HighSpecs],
    LowRules = [compile_alarm_spec(LS) || LS <- LowSpecs],
    SwitchRules = [compile_alarm_spec(SS) || SS <- SwitchSpecs],
    HighMatch1Rule = compile_alarm_spec(HighSpecMatch1),
    HighMatch2Rule = compile_alarm_spec(HighSpecMatch2),

    %% high rules
    [{"testing rules",
      [?_assertMatch(#rule{guard=HighAlarm, ttl=1, patterns=_}, hd(HighRules)),

       ?_assertEqual({high,off, 0.5}, apply_rule(MetricName1, 0.5, hd(HighRules))),
       ?_assertEqual({high, on, 4.5}, apply_rule(MetricName1, 4.5, hd(HighRules))),
       ?_assertEqual({high2, threshold, 4.91}, apply_rule(MetricName1, 4.91, hd(HighRules))),
       ?_assertEqual({high2,on, 5.1}, apply_rule(MetricName1, 5.1, hd(HighRules))),

       ?_assertEqual({high,off, 0.5}, apply_rule(MetricName2, 0.5, lists:nth(2, HighRules))),
       ?_assertEqual({high, on, 4.5}, apply_rule(MetricName2, 4.5, lists:nth(2, HighRules))),
       ?_assertEqual({high2, threshold, 4.91}, apply_rule(MetricName2, 4.91, lists:nth(2, HighRules))),
       ?_assertEqual({high2, on, 5.1}, apply_rule(MetricName2, 5.1, lists:nth(2, HighRules))),

       %% low rules
       ?_assertMatch(#rule{guard=LowAlarm, ttl=1, patterns=_}, hd(LowRules)),

       ?_assertEqual({low2, on, 0.5}, apply_rule(MetricName1, 0.5, hd(LowRules))),
       ?_assertEqual({low, on, 1.5}, apply_rule(MetricName1, 1.5, hd(LowRules))),
       ?_assertEqual({low2, threshold, 1.01}, apply_rule(MetricName1, 1.01, hd(LowRules))),
       ?_assertEqual({low, off, 3}, apply_rule(MetricName1, 3, hd(LowRules))),
       ?_assertEqual({low, on, 1.5}, apply_rule(MetricName2, 1.5, lists:nth(2, LowRules))),

       %% switch rules
       ?_assertEqual({switch, off, false}, apply_rule(MetricName1, false, hd(SwitchRules))),
       ?_assertEqual({switch, on, true}, apply_rule(MetricName1, true, hd(SwitchRules))),


       ?_assertEqual({high, on, 4.5}, apply_rule(MetricName1, 4.5, HighMatch1Rule)),
       ?_assertEqual({high, on, 4.5}, apply_rule(MetricName1, 4.5, HighMatch2Rule)),

       ?_assertMatch([{{low, off, _}, _}, {{high, on, _}, _}],
                     rules_find(MetricName1, 4.5, LowRules ++ HighRules)),
       ?_assertEqual(rules_find(MetricName1, 4.5, LowRules ++ HighRules),
                     rules_find(MetricName1, 4.5, HighRules ++ LowRules))
      ]
     }
    , {setup,
       fun () ->
               application:start(lager),
               application:start(gproc)
       end,
       fun (_) ->
               ok
       end,
       fun (_) ->
               [fun () ->
                        subscribe_alarm_events(),
                        Rules = HighRules ++ LowRules,
                        AA = active_alarms_new(),
                        %% no event
                        AA1 = process_port_val(MetricName1, 3, Rules, AA),
                        ?assertEqual([], active_alarms_list(AA1)),
                        Events1 = recieve_alarm_events(10),
                        ?assertMatch([], Events1),
                        %% low
                        AA2 = process_port_val(MetricName1, 1.5, Rules, AA1),
                        ?assertMatch([{MetricName1, {low, _}}], active_alarms_list(AA2)),
                        Events2 = recieve_alarm_events(10),
                        ?assertMatch([{MetricName1, _, low,on}], Events2),
                        %% not expired, no events
                        AA3 = process_port_val(MetricName1, 1.5, Rules, AA2),
                        ?assertMatch([{MetricName1, {low, _}}], active_alarms_list(AA3)),
                        Events3 = recieve_alarm_events(2000),
                        ?assertMatch([], Events3),
                        %% expired
                        AA4 = process_port_val(MetricName1, 1.5, Rules, AA3),
                        ?assertMatch([{MetricName1, {low, _}}], active_alarms_list(AA4)),
                        Events4 = recieve_alarm_events(10),
                        ?assertMatch([{MetricName1, _, low,on}], Events4),
                        %% low2
                        AA5 = process_port_val(MetricName1, 0.5, Rules, AA4),
                        ?assertMatch([{MetricName1, {low2, _}}], active_alarms_list(AA5)),
                        Events5 = recieve_alarm_events(10),
                        ?assertMatch([{MetricName1, _, low2, on}], Events5),
                        %% low
                        AA6 = process_port_val(MetricName1, 1.5, Rules, AA5),
                        ?assertMatch([{MetricName1, {low, _}}], active_alarms_list(AA6)),
                        Events6 = recieve_alarm_events(10),
                        ?assertMatch([{MetricName1, _, low2, off},
                                      {MetricName1, _, low, on}], Events6),
                        %% low2, then normal
                        AA7 = process_port_val(MetricName1, 0.5, Rules, AA6),
                        ?assertMatch([{MetricName1, {low2, _}}], active_alarms_list(AA7)),
                        AA8 = process_port_val(MetricName1, 3, Rules, AA7),
                        ?assertMatch([], active_alarms_list(AA8)),
                        Events7 = recieve_alarm_events(10),
                        ?assertMatch([{MetricName1, _, low2, on},
                                      {MetricName1, _, low2, off}], Events7),

                        %% low2, then high2
                        AA9 = process_port_val(MetricName1, 0.5, Rules, AA8),
                        ?assertMatch([{MetricName1, {low2, _}}], active_alarms_list(AA9)),
                        AA10 = process_port_val(MetricName1, 8, Rules, AA9),
                        ?assertMatch([{MetricName1, {high2, _}}], active_alarms_list(AA10)),
                        Events8 = recieve_alarm_events(10),
                        ?assertMatch([{MetricName1, _, low2, on},
                                      {MetricName1, _, high2, on},
                                      {MetricName1, _, low2, off}
                                     ], lists:sort(Events8)),
                        unsubscribe_alarm_events()
                end
               ]
       end
      }
    ].

process_port_val(MetricName, Val, Rules, ActiveAlarms) ->
    AE1 = rules_find(MetricName, Val, Rules),
    AA1 = lists:foldl(fun (AE, AA) ->
                              process_alarm_event(MetricName, AE, AA)
                      end,
                      ActiveAlarms, AE1),
    AA1.

recieve_alarm_events(Timeout) ->
    recieve_alarm_events(Timeout, []).

send_alarm_event(MetricName, Value, AlarmName, OnOff) ->
    gproc_ps:publish(l, metric_alarm, {MetricName, Value, AlarmName, OnOff}).

subscribe_alarm_events() ->
    gproc_ps:subscribe(l, metric_alarm).

unsubscribe_alarm_events() ->
    gproc_ps:unsubscribe(l, metric_alarm).

recieve_alarm_events(Timeout, Acc) ->
    receive
        {gproc_ps_event, metric_alarm, Msg} ->
            recieve_alarm_events(Timeout, [Msg|Acc])
    after Timeout ->
            lists:reverse(Acc)
    end.

-endif.
