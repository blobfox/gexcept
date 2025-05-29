import gexcept
import gleam/function
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
