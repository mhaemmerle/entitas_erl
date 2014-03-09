-record(entity, {id :: atom(),
                 creation_index = undefined :: undefined | integer(),
                 components = dict:new() :: dict(),
                 ctypes = sets:new() :: set()}).

-record(component, {type :: atom(),
                    impl :: any()}).

-record(repository, {entities = dict:new() :: dict(),
                     collections = dict:new() :: dict(),
                     collections_for_type = dict:new() :: dict(),
                     current_index = 0 :: integer()}).

-record(collection, {matcher :: function(),
                     entities = dict:new() :: dict(),
                     add_observers = sets:new() :: set(),
                     remove_observers = sets:new() :: set()}).

-record(system, {id :: atom(),
                 active :: boolean(),
                 activate_cb :: function(),
                 deactivate_cb :: function(),
                 execute_cb :: function()}).
