%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Disk load monitor
%%%
%%% @end
%%% Created : 11 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>
-module(eshaman_disk).

-behaviour(exometer_probe).

%% exometer_entry callbacks
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

-type min() :: integer().

-record(st, {
          datapoints,
          data,
          check_interval = 30 :: min(),
          full_threshold = 0.8,
          ref
         }).

behaviour() ->
    probe.

%% @doc Measure disk space usage in percents
%% .Options
%% check_interval -- period to check disks in minutes, default 30
%% full_threshold -- disk treated full after its _free_space < disk_capacity * full_threshold_
%% @end
probe_init(_, _, Opts) ->
    {[CheckIval, FullThreshold], _Opts1} =
        sutil:proplist_extract([check_interval, full_threshold],
                               sutil:proplist_defaults([{check_interval, 30}, {full_threshold, 0.8}],
                                                       Opts)),
    DP = [list_to_atom(K)
          || {K, _, _} <- disksup:get_disk_data(), not is_system_disk(K)],
    disksup:set_check_interval(CheckIval),
    disksup:set_almost_full_threshold(FullThreshold),
    {ok, #st{datapoints = DP,
             check_interval = CheckIval,
             full_threshold = FullThreshold
            }}.

probe_terminate(_) -> ok.

probe_get_value(DPs, #st{data = Data0,
                    datapoints = DPs0} = S) ->
    Data1 = if Data0 == undefined -> sample(DPs0);
               true -> Data0
            end,
    DPs1 = if DPs == default -> DPs0;
              true -> DPs
           end,
    {ok, probe_get_value_(Data1, DPs1), S#st{data = Data1}}.

probe_get_value_(Data, DPs) ->
    [D || {K,_} = D <- Data,
          lists:member(K, DPs)].

probe_get_datapoints(#st{datapoints = DPs}) ->
    {ok, DPs}.

probe_update(_, _) ->
    {error, not_supported}.

probe_reset(S) ->
    {ok, S#st{data = []}}.

probe_sample(#st{datapoints = DPs} = S) ->
    {_Pid, Ref} = spawn_monitor(
                    fun() ->
                            exit({sample, sample(DPs)})
                    end),
    {ok, S#st{ref = Ref}}.

probe_setopts(_Entry, Opts, S) ->
    DPs = proplists:get_value(datapoints, Opts, S#st.datapoints),
    {ok, S#st{datapoints = DPs}}.

probe_handle_msg({'DOWN', Ref, _, _, {sample,Data}}, #st{ref = Ref} = S) ->
    {ok, S#st{ref = undefined, data = Data}};

probe_handle_msg(_, S) ->
    {ok, S}.

probe_code_change(_, S, _) -> {ok, S}.

sample(DPs) ->
    [{list_to_atom(K), V} || {K, _, V} <- disksup:get_disk_data(),
                             lists:member(list_to_atom(K), DPs)].

is_system_disk(Name) ->
    lists:any(fun (SN) -> lists:prefix(SN, Name) end, ["/dev", "/sys", "/boot", "/run"]).
