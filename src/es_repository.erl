-module(es_repository).

-include("../include/es.hrl").

%% API
-export([add_entity/2,
         remove_entity/2,
         contains_entity/2,
         collection_for_matcher/3,
         entities_for_matcher/3,
         collection_for_types/2,
         add_component/3,
         exchange_component/3,
         remove_component/3]).

-export([new/0,
         entities/1,
         entities_to_list/1,
         collections/1,
         collections_for_type/1,
         current_index/1]).

%% -----------------------------------------------------------------------------
%% API

add_entity(Repository, Entity) ->
    CurrentIndex = current_index(Repository),
    NewEntity = es_entity:update_creation_index(CurrentIndex, Entity),
    NewEntities = dict:store(CurrentIndex, NewEntity, entities(Repository)),
    {Repository#repository{entities = NewEntities,
                           current_index = CurrentIndex + 1}, NewEntity}.

remove_entity(Repository, Entity) ->
    NewEntities = dict:erase(es_entity:creation_index(Entity)),
    Repository#repository{entities = NewEntities}.

contains_entity(Repository, Entity) ->
    case dict:find(es_entity:creation_index(Entity), entities(Repository)) of
        {ok, _} ->
            true;
        error ->
            false
    end.

collection_for_matcher(Repository, MType, CTypes) ->
    MKey = es_matcher:to_key(MType, CTypes),
    case dict:find(MKey, collections(Repository)) of
        {ok, Collection} ->
            {Repository, Collection};
        error ->
            MColl = memoize_matcher(Repository, MType, CTypes),
            NewCollections = dict:store(MKey, MColl, collections(Repository)),
            {Repository#repository{collections = NewCollections}, MColl}
    end.

entities_for_matcher(Repository, MType, CTypes) ->
    {NewRepository, Collection} = collection_for_matcher(Repository, MType, CTypes),
    {NewRepository, es_collection:entities(Collection)}.

collection_for_types(Repository, CTypes) ->
    MType = fun es_matcher:all_of_set/2,
    collection_for_matcher(Repository, MType, CTypes).

add_component(Repository, CType, Entity) ->
    NewEntities = dict:store(es_entity:creation_index(Entity), Entity, entities(Repository)),
    NewRepository = Repository#repository{entities = NewEntities},
    F = fun(Collection) ->
                case (es_collection:matcher(Collection))(es_entity:ctypes(Entity)) of
                    true ->
                        es_collection:add_entity(Collection, Entity);
                    false ->
                        Collection
                end
        end,
    InternalCollections = internal_collections_for_type(NewRepository, CType),
    NewInternalCollections = case length(InternalCollections) of
                                 0 ->
                                     Matcher = fun(B) -> es_matcher:just(CType, B) end,
                                     [F(es_collection:init_with_matcher(Matcher))];
                                 _ ->
                                     lists:map(F, InternalCollections)
                             end,
    NewCollectionsForType =
        dict:store(CType, NewInternalCollections, collections_for_type(Repository)),
    NewRepository#repository{collections_for_type = NewCollectionsForType}.

exchange_component(Repository, CType, Entity) ->
    F = fun(Collection) ->
                case (es_collection:matcher(Collection))(es_entity:ctypes(Entity)) of
                    true ->
                        es_collection:exchange_entity(Collection, Entity);
                    false ->
                        Collection
                end
        end,
    NewCollectionsForType =
        dict:update(CType, fun(C) -> lists:map(F, C) end, collections_for_type(Repository)),
    Repository#repository{collections_for_type = NewCollectionsForType}.

remove_component(Repository, CType, Entity) ->
    CTypes0 = [CType|es_entity:ctypes(Entity)],
    F = fun(Collection) ->
                Matcher = es_collection:matcher(Collection),
                IsOriginalMatch = Matcher(CTypes0),
                IsMatch = Matcher(es_entity:ctypes(Entity)),
                case IsOriginalMatch andalso IsMatch of
                    true ->
                        es_collection:remove_entity(Collection, Entity);
                    false ->
                        Collection
                end
        end,
    NewCollectionsForType =
        dict:update(CType, fun(C) -> lists:map(F, C) end, collections_for_type(Repository)),
    Repository#repository{collections_for_type = NewCollectionsForType}.

%% -----------------------------------------------------------------------------
%% Internal functions

internal_collections_for_type(Repository, CType) ->
    case dict:find(CType, collections_for_type(Repository)) of
        {ok, Collection} ->
            Collection;
        error ->
            sets:new()
    end.

memoize_matcher(Repository, MType, CTypes) ->
    Matcher = fun(B) ->
                      MType(CTypes, B)
              end,
    F = fun(Entity, Collection) ->
                case Matcher(es_entity:ctypes(Entity)) of
                    true ->
                        es_collection:add_entity(Collection, Entity);
                    false ->
                        Collection
                end
        end,
    lists:foldl(F, es_collection:init_with_matcher(Matcher), entities_to_list(Repository)).

%% -----------------------------------------------------------------------------
%% Accessors

new() ->
    #repository{}.

entities(#repository{entities = Entities}) ->
    Entities.

entities_to_list(Repository) ->
    lists:map(fun({_K, V}) -> V end, dict:to_list(entities(Repository))).

collections(#repository{collections = Collections}) ->
    Collections.

collections_for_type(#repository{collections_for_type = CollectionsForType}) ->
    CollectionsForType.

current_index(#repository{current_index = CurrentIndex}) ->
    CurrentIndex.
