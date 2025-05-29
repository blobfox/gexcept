//// This modul provides support for exceptions in Gleam for both Erlang and Javascript targets.

import gleam/option.{type Option, None, Some}

/// For handling exceptions
/// 
/// The `try_callback` will be executed immediately. Its return value is also returned from `try`.
/// Only in case `try_callback` calls `throw` the `catch_callback` will be called. Its argument is the argument
/// of `throw`. `try` will then return whatever `catch_callback` returns.
/// 
/// Note: The exception type is not checked.
@external(erlang, "gexcepterl", "try_ffi")
@external(javascript, "./gexceptjs.mjs", "try_ffi")
pub fn try(try_callback: fn() -> r, catch_callback: fn(e) -> r) -> r

/// For throwing exceptions
/// 
/// Calling `throw` will stop the executing of the current function. The control flow resumes in the `catch_callback`
/// of the next `try` call in the call stack.
/// 
/// Note: The exception type is not checked.
@external(erlang, "gexcepterl", "throw_ffi")
@external(javascript, "./gexceptjs.mjs", "throw_ffi")
pub fn throw(exception: e) -> a

/// For unwrapping results
/// 
/// This function will return the Ok-value of its parameter. In case the parameter is an Error, the argument of
/// the werror will be thrown instead.
pub fn unwrap_or_throw(to_unwrap: Result(v, e)) -> v {
  case to_unwrap {
    Ok(v) -> v
    Error(e) -> {
      throw(e)
    }
  }
}

/// For unwrapping options
/// 
/// This function will return the Some-value of its parameter. In case the parameter is None, `exception` argument
/// wil be thrown.
pub fn unwrap_option_or_throw(to_unwrap: Option(v), exception: e) -> v {
  case to_unwrap {
    Some(v) -> v
    None -> {
      throw(exception)
    }
  }
}
