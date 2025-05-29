-module(gexcepterl).
-export([try_ffi/2, throw_ffi/1, nil_value/0]).

try_ffi(TryCallback, CatchCallback) ->
    try TryCallback() of
        R -> R
    catch
        throw:E -> CatchCallback(E)
    end.


throw_ffi(E) ->
    throw(E).

nil_value() ->
    nil.