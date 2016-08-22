%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Ping histogram
%%%
%%% @end
%%% Created : 11 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_ping).
-behaviour(exometer_probe).

%% exometer_entry callbacks
-export([new/3,
         delete/3,
         get_value/3,
         get_value/4,
         get_datapoints/3,
         setopts/4,
         update/4,
         reset/3,
         sample/3]).

%% exometer_probe callbacks
-export([behaviour/0,
         probe_init/3,
         probe_terminate/1,
         probe_get_value/2,
         probe_get_datapoints/1,
         probe_update/2,
         probe_reset/1,
         probe_sample/1,
         probe_setopts/3,
         probe_handle_msg/2,
         probe_code_change/3]).

-include_lib("exometer/include/exometer.hrl").
-include("../include/log.hrl").

-type mcsec() :: integer().
-type ping_timeout() :: mcsec().
%% -type msec() :: integer().
-type duration() :: mcsec().

-record(st, {name,
             socket,
             timeout = infinity,
             mod,
             mod_opts,
             histogram,
             count = 0,
             last = 0,
             opts = [],
             worker_pid,
             worker_busy=false}).

-define(DATAPOINTS, [count, last]).
-define(DEFAULT_OPTS, [{sample_interval, 5000},
                       {time_span, 60000}]).

-callback probe_init([proplists:property()]) -> {ok, term()}.
-callback probe_ping(ping_timeout(), term()) -> {ok, duration(), term()} | {error, term(), term()}.

behaviour() ->
    entry.

%% exometer_entry callbacks
%% @doc Create ping probe instance
%% ++++
%% <p/>
%% ++++
%% It accepts all parameters of  _exometer_histogram_,
%% by default it sets _sample_interval_ to _5000_ and
%% _[{slot_period, 5000}, {time_span, 60000}]_ for  _exometer_histogram_,
%% _slot_period_ always equal to _sample_interval_.
%% @end
new(Name, Type, Options) ->
    Options1 = proplists:substitute_aliases([{span, time_span},
                                             {interval, sample_interval}],
                                            sutil:proplist_defaults(?DEFAULT_OPTS, Options)),
    SI = proplists:get_value(sample_interval, Options1),
    Span = proplists:get_value(time_span, Options1),
    Options2 = deepprops:set(timeout, Span, deepprops:set(slot_period, SI, Options1)),
    exometer_probe:new(Name, Type, [{arg, ?MODULE}|Options2]).

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

get_value(Name, Type, Ref) ->
    exometer_probe:get_value(Name, Type, Ref).

get_value(Name, Type, Ref, DataPoints) ->
    exometer_probe:get_value(Name, Type, Ref, DataPoints).

get_datapoints(Name, Type, Ref) ->
    exometer_probe:get_datapoints(Name, Type, Ref).

setopts(Name, Opts, Type, Ref) ->
    exometer_probe:setopts(Name, Opts, Type, Ref).

update(_Name, _Val, _Type, _Ref) ->
    {error, unsupported}.

reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

sample(_, _, _) ->
    {error, unsupported}.

probe_init(Name, Type, Options) ->
    sutil:proplist_require([module], Options),
    {[Mod, Timeout], Options1} = sutil:proplist_extract([module, timeout], Options),
    {ok, H} = exometer_histogram:probe_init(Name, Type, Options1),
    WorkerPid = spawn_worker(self(), Mod, Options1, Timeout),
    {ok, #st{ name = Name,
              mod = Mod,
              mod_opts = Options1,
              timeout = Timeout,
              histogram = H,
              worker_pid = WorkerPid}}.

probe_terminate(#st{socket=undefined}) ->
    ok;
probe_terminate(#st{socket=S}) ->
    gen_icmp:close(S),
    ok.

probe_get_datapoints(#st{histogram = H}) ->
    {ok, HDPs} = exometer_histogram:probe_get_datapoints(H),
    {ok, ?DATAPOINTS ++ HDPs}.

probe_get_value(DataPoints, #st{histogram = H, count = C, last = L} = St) ->
    HVals =
        case DataPoints -- ?DATAPOINTS of
            [] ->
                [];
            HDPs ->
                {ok, HVals1} = exometer_histogram:probe_get_value(HDPs, H),
                HVals1
        end,
    Res = [{K, V} || {K, V} <- [{count, C}, {last, L}|HVals],
                     lists:member(K, DataPoints) == true],
    {ok, Res, St}.

probe_setopts(_Entry, _Opts, _St) ->
    ok.

probe_update(_Value, _St) ->
    error(unsupported).

probe_reset(#st{histogram = H} = St) ->
    {ok, H1} = exometer_histogram:probe_reset(H),
    {ok, St#st{histogram = H1, count = 0, last = 0}}.

probe_sample(St=#st{worker_busy=true}) ->
    {ok, St};
probe_sample(St=#st{worker_busy=false, worker_pid=Worker}) ->
    Worker ! {self(), {sample}},
    {ok, St#st{worker_busy=true}}.

probe_code_change(_From, ModSt, _Extra) ->
    {ok, ModSt}.

probe_handle_msg({'DOWN', _, _, WorkerPid, Reason},
                 St=#st{worker_pid=WorkerPid, mod=Mod, name=N, mod_opts=Opts, timeout=T,
                        histogram=H}) ->
    ?error("~p ping ~p, error on sampling ~p", [Mod, N, Reason]),
    WorkerPid1 = spawn_worker(self(), Mod, Opts, T),
    Duration = -1,
    {ok, H1} = exometer_histogram:probe_update(Duration, H),
    {ok, St#st{histogram = H1, last = Duration,
               worker_busy=false, worker_pid=WorkerPid1}};
probe_handle_msg({sample_result, Duration}, St=#st{histogram=H, count=C}) ->
    {ok, H1} = exometer_histogram:probe_update(Duration, H),
    {ok, St#st{histogram = H1, count = C+1, last = Duration, worker_busy=false}};
probe_handle_msg({error, Error}, St=#st{mod=Mod, name=N, histogram=H, count=C}) ->
    ?error("~p ping failed for ~p, error ~p", [Mod, N, Error]),
    Duration = -1,
    {ok, H1} = exometer_histogram:probe_update(Duration, H),
    {ok, St#st{histogram = H1, count = C+1, last = Duration, worker_busy=false}};
probe_handle_msg(_, S) ->
    {ok, S}.

spawn_worker(Parent, Mod, Opts, Timeout) ->
    {Pid, _Ref} =
        spawn_monitor(
          fun() ->
                  {ok, State} = Mod:probe_init(Opts),
                  erlang:monitor(process, Parent),
                  worker_loop(Parent, Mod, Timeout, State)
          end),
    Pid.

worker_loop(Parent, Mod, Timeout, State) ->
    NxtState =
        receive
            {'DOWN', _Ref, _, Parent, Reason} ->
                exit(Reason);
            {Parent, {sample}} ->
                case Mod:probe_ping(Timeout, State) of
                    {ok, Duration, State1} ->
                        Parent ! {sample_result, Duration},
                        State1;
                    {error, Error, State1} ->
                        Parent ! {error, Error},
                        State1
                end
        end,
    worker_loop(Parent, Mod, Timeout, NxtState).
