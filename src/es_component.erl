-module(es_component).

-include("../include/es.hrl").

-export([type/1]).

type(#component{type = CType}) ->
    CType.
