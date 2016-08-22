%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_mail).

-behaviour(gen_server).

-export([start_link/1, send/4, send_alarm/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(SRV, ?MODULE).

-record(state, {mail_opts, from_address}).

start_link(Args) ->
    gen_server:start_link({local, ?SRV}, ?MODULE, Args, []).

send_alarm(Metric, Value, Alarm, Toggle, ToAddrs) ->
    Subject =  string:strip(eshaman_util:format_alarm(Metric, Value, Alarm, Toggle, " ")),
    Msg = "Please don't answer this mail.\r\n"
        ++ "This is automatic notification.\r\n"
        ++ "---\r\n"
        ++ "With love,\r\n"
        ++ "Your Oldy Watchdoggy\r\n"
        ++ "                 .  \r\n"
        ++ "                / V\\\r\n"
        ++ "              / `  /\r\n"
        ++ "             <<   | \r\n"
        ++ "             /    | \r\n"
        ++ "           /      | \r\n"
        ++ "         /        | \r\n"
        ++ "       /    \\  \\ /  \r\n"
        ++ "      (      ) | |  \r\n"
        ++ "   ___|   _/_  | |  \r\n"
        ++ " <_____\\______)\\__) \r\n",
    send(undefined, ToAddrs, Subject, Msg).

send(FromAddr, ToAddrs=[ToAddr|_], Subject, Body) ->
    MessageHeader = build_message_header([{"Subject", Subject},
                                          {"To", ToAddr},
                                          {"From", ""}],
                                         "text/plain",
                                         "utf-8"),
    Body1 = iolist_to_binary([MessageHeader, "\r\n", convert_unix_newlines_to_dos(Body)]),
    gen_server:call(?SRV, {deliver, FromAddr, ToAddrs, Body1}).

init(Opts) ->
    sutil:proplist_require([from_address], Opts),
    MailOpts = [{K, V}
                || {K, V} <- [{K, proplists:get_value(K, Opts)}
                              || K <- [relay, username, password]],
                   V =/= undefined],
    eshaman_node_adm:register_service(smtp, self()),
    {ok, #state{mail_opts=MailOpts,
                from_address=proplists:get_value(from_address, Opts)}}.

handle_call({deliver, FromAddr, ToAddr, Body}, _From, State) ->
    {reply, do_deliver(State, FromAddr, ToAddr, Body), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_deliver(#state{mail_opts=MO, from_address=FA}, FromAddr, ToAddrs=[To|_], Body) ->
    MailOpts =
        case proplists:lookup(relay, MO) of
            undefined ->
                [_User, Host] = string:tokens(To, "@"),
                [{relay, Host} | MO];
            _ ->
                MO
        end,
    FromAddr1 = if FromAddr == undefined ->
                        FA;
                   true ->
                        FromAddr
                end,
    Email = {FromAddr1, ToAddrs, Body},
    ResF = sutil:curry(fun report_delivery_result/3, [FromAddr, ToAddrs]),
    gen_smtp_client:send(Email, MailOpts, ResF).

report_delivery_result({ok, _}, _From, _To) ->
    ok;
report_delivery_result({error, Type, Message}, From, To) ->
    ?error("delivering from ~p to ~p ~p ~p", [From, To, Type, Message]);
report_delivery_result({exit, Reason}, From, To) ->
    ?error("delivering from ~p to ~p ~p", [From, To, Reason]).

build_message_header(HeaderFields, DefaultMimeType, CharSet) ->
    MessageID = smtp_util:generate_message_id(),
    CT = proplists:get_value("Content-Type", HeaderFields, DefaultMimeType),
    ContentType = build_content_type(CT, CharSet),
    DefDate = strftime:f(os:timestamp(), "%a, %d %b %Y %T +0000", universal),
    Date = proplists:get_value("Date", HeaderFields, DefDate),
    AllHeaders = [{"Date", Date},
                  {"Content-Type", ContentType},
                  {"MIME-Version", "1.0"},
                  {"Message-ID", MessageID} | HeaderFields],
    add_fields(AllHeaders, [], []).


build_content_type(ContentType, CharSet) ->
    case CharSet of
        undefined ->
            ContentType;
        _ ->
            io_lib:format("~s; charset=~s", [ContentType, CharSet])
end.

add_fields([], _, Acc) ->
    lists:reverse(Acc);
add_fields([{Key, Value}|Rest], Seen, Acc) ->
    case proplists:get_value(Key, Seen) of
        undefined ->
            add_fields(Rest, [Key|Seen], [[Key, ": ", Value, "\r\n"] | Acc]);
        _ ->
            add_fields(Rest, Seen, Acc)
    end.

convert_unix_newlines_to_dos(Body) when is_binary(Body) ->
    convert_unix_newlines_to_dos(binary_to_list(Body));
convert_unix_newlines_to_dos(Body) when is_list(Body) ->
    convert_unix_newlines_to_dos(Body, []).

convert_unix_newlines_to_dos([], Acc) ->
    lists:reverse(Acc);
convert_unix_newlines_to_dos([$\r, $\n|Rest], Acc) ->
    convert_unix_newlines_to_dos(Rest, [$\n, $\r|Acc]);
convert_unix_newlines_to_dos([$\n|Rest], Acc) ->
    convert_unix_newlines_to_dos(Rest, [$\n, $\r|Acc]);
convert_unix_newlines_to_dos([H|T], Acc) when is_binary(H); is_list(H) ->
    convert_unix_newlines_to_dos(T, [convert_unix_newlines_to_dos(H)|Acc]);
convert_unix_newlines_to_dos([H|T], Acc) ->
    convert_unix_newlines_to_dos(T, [H|Acc]).
