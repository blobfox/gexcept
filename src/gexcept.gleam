import gleam/option.{type Option, None, Some}

@external(erlang, "gexcepterl", "try_ffi")
@external(javascript, "./gexceptjs.mjs", "try_ffi")
pub fn try(try_callback: fn() -> r, catch_callback: fn(e) -> r) -> r

@external(erlang, "gexcepterl", "throw_ffi")
@external(javascript, "./gexceptjs.mjs", "throw_ffi")
pub fn throw(exception: e) -> Nil

@external(erlang, "gexcepterl", "nil_value")
@external(javascript, "./gexceptjs.mjs", "nil_value")
fn generic_nil_value() -> a

pub fn unwrap_or_throw(to_unwrap: Result(v, e)) -> v {
  case to_unwrap {
    Ok(v) -> v
    Error(e) -> {
      throw(e)
      generic_nil_value()
    }
  }
}

pub fn unwrap_option_or_throw(to_unwrap: Option(v), exception: e) -> v {
  case to_unwrap {
    Some(v) -> v
    None -> {
      throw(exception)
      generic_nil_value()
    }
  }
}
