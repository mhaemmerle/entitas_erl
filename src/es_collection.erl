-module(es_collection).

-include("../include/es.hrl").

-export([init_with_matcher/1,
         init_with_types/1,
         add_entity/2,
         exchange_entity/2,
         remove_entity/2]).

-export([new/1,
         entities/1,
         matcher/1]).

%% -----------------------------------------------------------------------------
%% API

init_with_matcher(Matcher) ->
    new(Matcher).

init_with_types(CTypes) ->
    new(fun(B) ->
                es_matcher:all_of(CTypes, B)
        end).

add_entity(Collection, Entity) ->
    CreationIndex = es_entity:creation_index(Entity),
    case dict:find(CreationIndex, entities(Collection)) of
        error ->
            do_add_entity(Collection, CreationIndex, Entity);
        {ok, _} ->
            Collection
    end.

exchange_entity(Collection, Entity) ->
    CreationIndex = es_entity:creation_index(Entity),
    case dict:find(CreationIndex, entities(Collection)) of
        error ->
            do_add_entity(Collection, CreationIndex, Entity);
        {ok, _} ->
            Collection
    end.

remove_entity(Collection, Entity) ->
    CreationIndex = es_entity:creation_index(Entity),
    Entities = entities(Collection),
    case dict:find(CreationIndex, Entities) of
        error ->
            Collection;
        {ok, _} ->
            Collection#collection{entities = dict:erase(CreationIndex, Entities)}
    end.

%% -----------------------------------------------------------------------------
%% Internal functions

do_add_entity(Collection, Index, Entity) ->
    NewEntities = dict:store(Index, Entity, entities(Collection)),
    Collection#collection{entities = NewEntities}.

%% -----------------------------------------------------------------------------
%% Accessors

new(Matcher) ->
    #collection{matcher = Matcher}.

entities(#collection{entities = Entities}) ->
    Entities.

matcher(#collection{matcher = Matcher}) ->
    Matcher.
