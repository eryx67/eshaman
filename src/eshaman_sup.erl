-module(eshaman_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD1(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link({IsMaster, Dist, Alarms, Mail, XMPP, SMS}) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {IsMaster, Dist, Alarms, Mail, XMPP, SMS}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({IsMaster, Dist, Alarms, Mail, XMPP, SMS}) ->
    DistSrv = ?CHILD1(eshaman_node_adm, worker, [Dist]),
    AlarmsSrv = ?CHILD1(eshaman_alarm, worker, [Alarms]),

    Services = [?CHILD1(Mod, worker, [Conf])
                || {Mod, Conf} <- [{eshaman_mail, Mail},
                                   {eshaman_xmpp, XMPP},
                                   {eshaman_sms, SMS}],
                   proplists:get_value(enabled, Conf, false) == true],
    if IsMaster ->
            {ok, { {one_for_one, 5, 10}, [DistSrv|Services] ++ [AlarmsSrv]} };
       true ->
            {ok, { {one_for_one, 5, 10}, [DistSrv, AlarmsSrv]} }
       end.
