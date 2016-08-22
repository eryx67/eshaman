%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 05 Oct 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_recs).

-compile({parse_transform, exprecs}).

-export([new/1, to_json/1]).

-export_records([high_alarm, low_alarm, switch_alarm, alarm_rule, metric_alarm, rule]).

-include("../include/eshaman.hrl").

to_json(Rec) ->
    json_rec:to_json(Rec, ?MODULE, eep).

new(_) -> error("must be implemented").
