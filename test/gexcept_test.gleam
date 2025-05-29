import gexcept
import gleam/function
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn try_no_exception_test() {
  gexcept.try(fn() { "expected result" }, fn(_) { "unexpected result" })
  |> should.equal("expected result")
}

pub fn try_exception_test() {
  gexcept.try(
    fn() {
      gexcept.throw("the exception to be caught")

      "the function return value that's not used"
    },
    function.identity,
  )
  |> should.equal("the exception to be caught")
}

fn nested_function() {
  gexcept.throw("from nested")
}

pub fn try_exception_from_nested_function_test() {
  gexcept.try(
    fn() {
      nested_function()
      "return value"
    },
    function.identity,
  )
  |> should.equal("from nested")
}

pub fn try_catch_block_gets_evaluated_test() {
  gexcept.try(
    fn() {
      gexcept.throw(21)

      1337
    },
    fn(e) { e * 2 },
  )
  |> should.equal(42)
}

pub fn try_nested_try_blocks_do_not_interfer_test() {
  gexcept.try(
    fn() {
      gexcept.try(fn() { "no exception" }, fn(_) { "should not be called" })
      <> gexcept.try(
        fn() {
          gexcept.throw(" exception")
          "return"
        },
        function.identity,
      )
    },
    fn(_) { "should not be called" },
  )
  |> should.equal("no exception exception")
}

pub fn try_catch_block_throws_again_test() {
  gexcept.try(
    fn() {
      gexcept.try(
        fn() {
          gexcept.throw("exception")
          "try 1 return"
        },
        fn(e) {
          gexcept.throw("catch 1: " <> e)
          "catch 1 return"
        },
      )
      "try 2 return"
    },
    fn(e) { "catch 2: " <> e },
  )
  |> should.equal("catch 2: catch 1: exception")
}

pub fn unwrap_or_throw_on_ok_test() {
  gexcept.try(fn() { gexcept.unwrap_or_throw(Ok("ok value")) }, fn(_) {
    "should not be called"
  })
  |> should.equal("ok value")
}

pub fn unwrap_or_throw_on_error_test() {
  gexcept.try(fn() { gexcept.unwrap_or_throw(Error("error value")) }, fn(e) {
    "caught: " <> e
  })
  |> should.equal("caught: error value")
}

pub fn unwrap_option_or_throw_on_some_test() {
  gexcept.try(
    fn() { gexcept.unwrap_option_or_throw(Some("some value"), "exception") },
    fn(_) { "should not be called" },
  )
  |> should.equal("some value")
}

pub fn unwrap_option_or_throw_on_none_test() {
  gexcept.try(fn() { gexcept.unwrap_option_or_throw(None, "exception") }, fn(e) {
    "caught: " <> e
  })
  |> should.equal("caught: exception")
}
