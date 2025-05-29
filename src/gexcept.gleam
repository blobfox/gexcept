@external(erlang, "gexcepterl", "try_ffi")
@external(javascript, "./gexceptjs.mjs", "_try")
pub fn try(try_callback: fn() -> r, catch_callback: fn(e) -> r) -> r

@external(erlang, "gexcepterl", "throw_ffi")
@external(javascript, "./gexceptjs.mjs", "_throw")
pub fn throw(exception: e) -> Nil
