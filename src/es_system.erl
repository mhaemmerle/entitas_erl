-module(es_system).

-include("../include/es.hrl").

%% API
-export([execute/1,
         activate/1,
         deactivate/1]).

-export([new/1,
         id/1,
         execute_cb/1,
         activate_cb/1,
         deactivate_cb/1]).

%% -----------------------------------------------------------------------------
%% API

execute(System) ->
    (execute_cb(System)).

activate(System) ->
    (activate_cb(System)),
    System#system{active = true}.

deactivate(System) ->
    (deactivate_cb(System)),
    System#system{active = false}.

%% -----------------------------------------------------------------------------
%% Accessors

new(Id) -> #system{id = Id}.

id(#system{id = Id}) ->
    Id.

execute_cb(#system{execute_cb = ExecuteCb}) ->
    ExecuteCb.

activate_cb(#system{activate_cb = ActivateCb}) ->
    ActivateCb.

deactivate_cb(#system{deactivate_cb = DeactivateCb}) ->
    DeactivateCb.

%% -----------------------------------------------------------------------------
%% Internal functions
