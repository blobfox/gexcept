-module(gexcepterl).
-export([try_ffi/2, throw_ffi/1]).

try_ffi(Try_callback, Catch_callback) ->
    try Try_callback() of
        R -> R
    catch
        throw:E -> Catch_callback(E)
    end.


throw_ffi(E) ->
    throw(E).
