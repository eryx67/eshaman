%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2013, Vladimir G. Sekissov
%%% @doc
%%% Параметры окружения приложения
%%% @end
%%% Created : 28 Oct 2013 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(eshaman_env).

-export([get/1, get/2, set/2]).

-define(APP, eshaman).

-spec get(atom()) -> {ok, term()} | undefined.
get(Key) ->
    Default = make_ref(),
    case gproc:get_env(l, ?APP, Key, [app_env, {default, Default}]) of
        Default ->
            undefined;
        Val ->
            {ok, Val}
    end.

-spec get(atom(), term()) -> term().
get(Key, Default) ->
    gproc:get_set_env(l, ?APP, Key, [app_env, {default, Default}]).

-spec set(atom(), term()) -> any().
set(Key, Value) ->
    gproc:set_env(l, ?APP, Key, Value, [app_env]).
