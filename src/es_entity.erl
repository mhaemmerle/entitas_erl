-module(es_entity).

-include("../include/es.hrl").

%% API
-export([new/1,
         has_component_of_type/2,
         has_components_of_types/2,
         component_of_type/2,
         contains_component/2,
         add_component/2,
         exchange_component/2,
         remove_component_of_type/2]).

-export([id/1,
         creation_index/1,
         components/1,
         ctypes/1,
         update_creation_index/2]).

%% -----------------------------------------------------------------------------
%% API

-spec(has_component_of_type(Entity::#entity{}, CType::atom()) -> boolean()).
has_component_of_type(Entity, CType) ->
    sets:is_element(CType, ctypes(Entity)).

-spec(has_components_of_types(Entity::#entity{}, CTypes::[atom()]) -> boolean()).
has_components_of_types(Entity, CTypes) ->
    sets:is_subset(CTypes, ctypes(Entity)).

component_of_type(Entity, CType) ->
    case dict:find(CType, components(Entity)) of
        {ok, Component} ->
            Component;
        error ->
            undefined
    end.

-spec(contains_component(Entity::#entity{}, Component::list()) -> boolean()).
contains_component(Entity, Component) ->
    case component_of_type(Entity, es_component:type(Component)) of
        undefined ->
            false;
        _ ->
            true
    end.

add_component(Entity, Component) ->
    case has_component_of_type(Entity, es_component:type(Component)) of
        true ->
            Entity;
        false ->
            do_add_component(Entity, Component)
    end.

exchange_component(Entity, Component) ->
    do_add_component(Entity, Component).

remove_component_of_type(Entity, CType) ->
    Entity#entity{components = dict:erase(CType, components(Entity)),
                  ctypes = sets:del_element(CType, ctypes(Entity))}.

%% -----------------------------------------------------------------------------
%% Accessors

new(Id) -> #entity{id = Id}.
        
id(#entity{id = Id}) ->
    Id.

creation_index(#entity{creation_index = CreationIndex}) ->
    CreationIndex.

components(#entity{components = Components}) ->
    Components.

ctypes(#entity{ctypes = CTypes}) ->
    CTypes.

update_creation_index(CreationIndex, Entity) ->
    Entity#entity{creation_index = CreationIndex}.

%% -----------------------------------------------------------------------------
%% Internal functions

do_add_component(Entity, Component) ->
    CType = es_component:type(Component),
    Entity#entity{components = dict:store(CType, Component, components(Entity)),
                  ctypes = sets:add_element(CType, ctypes(Entity))}.
