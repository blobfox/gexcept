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
pub fn throw(exception: e) -> Nil

/// For tricking the Gleam type system
/// 
/// In order for `unwrap_or_throw` and `unwrap_option_or_throw` to work, we need to be able to construct an instance 
/// of a generic type. This is not possible. However, since we can guarentee that this instance will never be used
/// we can instead of Erlang/Javascripts weaker type system to create a Nil value that's typed as a generic.
/// 
/// Be very careful with this one!
@external(erlang, "gexcepterl", "nil_value")
@external(javascript, "./gexceptjs.mjs", "nil_value")
fn generic_nil_value() -> a

/// For unwrapping results
/// 
/// This function will return the Ok-value of its parameter. In case the parameter is an Error, the argument of
/// the werror will be thrown instead.
pub fn unwrap_or_throw(to_unwrap: Result(v, e)) -> v {
  case to_unwrap {
    Ok(v) -> v
    Error(e) -> {
      throw(e)
      generic_nil_value()
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
      generic_nil_value()
    }
  }
}
