-module(es_matcher).

-include("../include/es.hrl").

-export([all_matching/2,
         any_matching/2,
         equal/2,
         all_of/2,
         any_of/2,
         just/2,
         to_key/2]).

all_matching(A, B) ->
    sets:is_subset(A, B).

any_matching(A, B) ->
    sets:size(sets:intersection(A, B)) > 0.

equal(A, B) ->
    A =:= B.

all_of(A, B) when is_list(B) ->
    all_matching(A, sets:from_list(B));
all_of(A, B) ->
    all_matching(A, B).

any_of(A, B) when is_list(B) ->
    any_matching(A, sets:from_list(B));
any_of(A, B) ->
    any_matching(A, B).

just(A, B) ->
    all_matching(A, set:from_list([B])).

to_key(MType, CType) when is_atom(CType) ->
    to_key(MType, [CType]);
to_key(MType, CTypes) ->
    L = lists:map(fun(E) when is_atom(E) -> erlang:atom_to_binary(E, utf8);
                     (E) -> E
                  end, [MType|CTypes]),
    erlang:phash2(erlang:list_to_binary(L)).
