-module(es).

-include("../include/es.hrl").

-export([add_component/3,
         exchange_component/3,
         remove_component/3,
         destroy_entity/2]).

add_component(Repository, Entity, Component) ->
    NewEntity = es_entity:add_component(Entity, Component),
    CType = es_component:type(Component),
    {es_repository:add_component(Repository, CType, NewEntity), NewEntity}.

exchange_component(Repository, Entity, Component) ->
    NewEntity = es_entity:exchange_component(Entity, Component),
    CType = es_component:type(Component),
    {es_repository:exchange_component(Repository, CType, NewEntity), NewEntity}.

remove_component(Repository, Entity, CType) ->
    NewEntity = es_entity:remove_component(Entity, CType),
    {es_repository:remove_component(Repository, CType, NewEntity), NewEntity}.

destroy_entity(Repository, Entity) ->
    lists:foldl(fun(CType, {Repository0, Entity0}) ->
                        remove_component(Repository0, Entity0, CType)
                end, {Repository, Entity}, es_entity:ctypes(Entity)).
