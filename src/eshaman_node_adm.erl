%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Master node registration and workers monitor
%%%
%%% @end
%%% Created : 11 Oct 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_node_adm).

-behaviour(gen_server).

-export([start_link/1, get_masters/0, register_service/2, whereis_service/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(SRV, ?MODULE).
-define(SVC_GROUP(Group, Svc), {Group, Svc}).

-define(PING_INTERVAL, 60000).

-record(state, {is_master=false,
                group,
                workers=[],
                ping_interval = ?PING_INTERVAL,
                pinger_ref
               }).

get_masters() ->
    gen_server:call(?SRV, {get_masters}, infinity).

register_service(Name, Pid) ->
    gen_server:call(?SRV, {register_service, Pid, Name}, infinity).

whereis_service(Name) ->
    gen_server:call(?SRV, {whereis_service, Name}, infinity).

start_link(Args) ->
    gen_server:start_link({local, ?SRV}, ?MODULE, Args, []).

init(Opts) ->
    {[IsMaster, Workers], _} = sutil:proplist_extract([is_master, nodes],
                                 sutil:proplist_validate(Opts,
                                                         [is_master, nodes],
                                                         [],
                                                         [{is_master, true},
                                                          {nodes, []}
                                                         ])),
    PingInterval = proplists:get_value(ping_interval, Opts, ?PING_INTERVAL),
    {ok, App} = application:get_application(),
    Group = proplists:get_value(group, Opts, App),
    Services = proplists:get_value(services, Opts, []),
    pg2:create(Group),
    [pg2:create(?SVC_GROUP(Group, Svc)) || Svc <- Services],
    [pg2:join(Group, self()) || IsMaster],
    State = #state{is_master=IsMaster, group=Group, workers=Workers,
                   ping_interval=PingInterval},
    if IsMaster ->
            {ok, State, 0};
       true ->
            {ok, State}
    end.

handle_call({get_masters}, _From, S=#state{group=Group}) ->
    {reply, lists:usort([node(Pid) || Pid <- pg2:get_members(Group)]), S};
handle_call({whereis_service, Name}, _From, S=#state{group=Group}) ->
    Node = node(),
    Nodes = lists:usort(fun (N1, _N2) when N1 == Node -> true;
                            (_N1, N2) when N2 == Node -> false;
                            (N1, N2) -> N1 =< N2
                        end,
                        [node(Pid) || Pid <- pg2:get_members(?SVC_GROUP(Group, Name))]),
    {reply, Nodes, S};
handle_call({register_service, _Pid, _Name}, _From, S=#state{is_master=false}) ->
    {reply, ok, S};
handle_call({register_service, Pid, Name}, _From, S=#state{group=Group, is_master=true}) ->
    Res = pg2:join(?SVC_GROUP(Group, Name), Pid),
    {reply, Res, S}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, S=#state{is_master=true,
                              pinger_ref=undefined,
                              ping_interval=PI,
                              workers=Ws}) ->
    {noreply, S#state{pinger_ref=spawn_pinger(PI, Ws)}};
handle_info({'DOWN', Ref, _, _, Reason}, S=#state{is_master=true,
                                                  pinger_ref=Ref,
                                                  ping_interval=PI,
                                                  workers=Ws}) ->
    ?error("pinger down ~p", [Reason]),
    {noreply, S#state{pinger_ref=spawn_pinger(PI, Ws)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

spawn_pinger(PollInterval, Workers) ->
    {_, Ref} = spawn_monitor(fun () -> start_pinger(PollInterval, Workers) end),
    Ref.

start_pinger(PollInterval, Workers) ->
    [net_kernel:connect_node(W) || W <- Workers],
    receive
    after PollInterval ->
            start_pinger(PollInterval, Workers)
    end.
