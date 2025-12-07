-record(state, {
    children = [] :: [{atom(), pid()}],
    permanent = [] :: [pid()]
}).

-record(params, {
    name :: atom(),
    restart = temporary :: permanent | temporary
}).