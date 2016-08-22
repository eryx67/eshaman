%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Memory load monitor
%%%
%%% @end
%%% Created : 11 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>
-module(eshaman_mem).

-behaviour(exometer_probe).

%% exometer_entry callbacks
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
-export(
   [
    behaviour/0,
    probe_init/3,
    probe_terminate/1,
    probe_get_value/2,
    probe_get_datapoints/1,
    probe_update/2,
    probe_reset/1,
    probe_sample/1,
    probe_setopts/3,
    probe_handle_msg/2,
    probe_code_change/3
   ]).

-include_lib("exometer/include/exometer.hrl").

-define(DATAPOINTS, [total, used, capacity, buffered, cached]).
-define(DEFAULT_OPTS, [{sample_interval, 5000},
                       {time_span, 60000}]).

-record(st, {
          data,
          histogram,
          ref
         }).

behaviour() ->
    entry.

new(Name, Type, Options) ->
    Options1 = proplists:substitute_aliases([{span, time_span},
                                             {interval, sample_interval}],
                                            sutil:proplist_defaults(?DEFAULT_OPTS, Options)),
    exometer_probe:new(Name, Type, [{arg, ?MODULE}|Options1]).

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

probe_init(Name, Type, Opts) ->
    {ok, H} = exometer_histogram:probe_init(Name, Type, Opts),
    {ok, #st{histogram = H}}.

probe_terminate(_) -> ok.

probe_get_value(DPs, #st{data = Data, histogram = H} = St) ->
    {Data1, H1} =
        if Data == undefined ->
                Data2 = sample(),
                C = proplists:get_value(capacity, Data2),
                {ok, H2} = exometer_histogram:probe_update(C, H),
                {Data2, H2};
           true ->
                {Data, H}
        end,
    HVals =
        case DPs -- ?DATAPOINTS of
            [] ->
                [];
            HDPs ->
                {ok, HVals1} = exometer_histogram:probe_get_value(HDPs, H1),
                HVals1
        end,
    Res = [{K, V} || {K, V} <- Data1 ++ HVals, lists:member(K, DPs) == true],
    {ok, Res, St#st{data=Data1, histogram=H1}}.

probe_get_datapoints(#st{histogram = H}) ->
    {ok, HDPs} = exometer_histogram:probe_get_datapoints(H),
    {ok, ?DATAPOINTS ++ HDPs}.

probe_setopts(_Entry, _Opts, _St) ->
    ok.

probe_update(_Value, _St) ->
    error(unsupported).

probe_reset(#st{histogram = H} = St) ->
    {ok, H1} = exometer_histogram:probe_reset(H),
    {ok, St#st{histogram = H1, data=undefined}}.

probe_sample(#st{} = S) ->
    {_Pid, Ref} = spawn_monitor(
                    fun() ->
                            exit({sample, sample()})
                    end),
    {ok, S#st{ref = Ref}}.

probe_handle_msg({'DOWN', Ref, _, _, {sample,Data}}, #st{ref = Ref, histogram=H} = S) ->
    C = proplists:get_value(capacity, Data),
    {ok, H1} = exometer_histogram:probe_update(C, H),
    {ok, S#st{ref = undefined, data = Data, histogram=H1}};
probe_handle_msg(_, S) ->
    {ok, S}.

probe_code_change(_, S, _) -> {ok, S}.

sample() ->
    KVs = memsup:get_system_memory_data(),
    [Total, Buffered, Cached, Free] =
        lists:map(fun (K) -> proplists:get_value(K, KVs, 0) end,
                  [system_total_memory, buffered_memory, cached_memory, free_memory]),
    UsedTmp = Total - Free - Buffered - Cached,
    Used = if UsedTmp == 0 ->
                   Total - Free - Buffered;
              true ->
                   UsedTmp
           end,
    [{total, Total/1000000}, {used, Used/1000000}, {capacity, (Used * 100) div Total},
     {buffered, Buffered/1000000}, {cached, Cached/1000000}].
