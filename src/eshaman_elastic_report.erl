%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Report exometer alarms to _eshaman_alarm_
%%%
%%% @end
%%% Created : 29 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_elastic_report).

-behaviour(exometer_report).

-include_lib("exometer/include/exometer.hrl").
-include_lib("erlastic_search/include/erlastic_search.hrl").
-include("eshaman.hrl").
-include("log.hrl").

%% gen_server callbacks
-export([cast_report/4]).

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

-record(st, {elastic :: #erls_params{},
             index_format :: string(),
             index :: binary()
            }).

cast_report(Metric, DataPoint, Extra, Value) ->
    exometer_report:cast_reporter(?MODULE, {report, Metric, DataPoint, Extra, Value}).

%%%===================================================================
%%% Probe callbacks
%%%===================================================================

exometer_init(Props) ->
    {[{Host, Port}, IndexFmt],_} =
        sutil:proplist_extract([host, index_format],
                               sutil:proplist_defaults([{host, {"127.0.0.1", 9200}},
                                                        {index_format, "shaman-%Y.%m.%d"}
                                                       ],
                                                       Props)),
    eshaman_node_adm:register_service(elastic, self()),
    ElasticState = #erls_params{host=sutil:ensure_binary(Host), port=Port},
    {ok, #st{elastic=ElasticState, index_format=IndexFmt}}.

exometer_report(Metric, DataPoint, Extra, Value, #st{index_format=IdxFmt, index=LastIdx} = St) ->
    Now = os:timestamp(),
    Idx = iolist_to_binary(strftime:f(Now, IdxFmt, universal)),
    Timestamp = iolist_to_binary(strftime:f(Now, "%Y-%m-%dT%H:%M:%S.000Z", universal)),
    [Dom, Group, Name, MetricN] = [atom_to_binary(V, utf8) || V <- Metric],
    PointEntries = case proplists:get_value(publish_name, Extra) of
                       undefined -> [{atom_to_binary(DataPoint, utf8), Value}];
                       PN -> [{atom_to_binary(PN, utf8), atom_to_binary(DataPoint, utf8)},
                              {<<"value">>, Value}]
                   end,
    ElDoc = {[{<<"@timestamp">>, Timestamp},
              {<<"domain">>, Dom},
              {<<"group">>, Group},
              {<<"name">>, Name}
             ] ++ PointEntries},
    case sutil:'maybe->'(
           Idx,
           [fun (I) when I =/= LastIdx ->
                    ok = create_index(I, St),
                    I;
                (I) ->
                    I
            end,
            fun (I) ->
                    publish_metric(I, MetricN, ElDoc, St)
            end,
            fun (_) ->
                    St#st{index=Idx}
            end
           ]) of
        Ret={ok, _} ->
            Ret;
        {error, Error} ->
            ?error("insert metric ~p to index ~p ~p", [ElDoc, Idx, Error]),
            {ok, St}
    end.

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
create_index(Name, #st{elastic=Els}) ->
    case erlastic_search:create_index(Els, Name) of
        {ok, _} ->
            ok;
        Error={error, {400, KVs=[{_, _}|_]}} ->
            Reason = proplists:get_value(<<"error">>, KVs),
            case binary:match(Reason, <<"exists">>) of
                nomatch ->
                    Error;
                _ ->
                    ok
            end;
        Error={error, _} ->
            Error
    end.

publish_metric(Idx, Type, Doc, #st{elastic=Els}) ->
    {ok, _} = erlastic_search:index_doc(Els, Idx, Type, jiffy:encode(Doc)).
