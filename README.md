# gexcept

[![Package Version](https://img.shields.io/hexpm/v/gexcept)](https://hex.pm/packages/gexcept)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gexcept/)

Ever wanted to use exceptions in Gleam? Now you can!

`gexcept` is a gleam library that provides generic functions to exception handling in Gleam. 

`gexcept.try()` takes two arguments: The try-callback - the function that might throw an exception - and the 
catch-callback -  exception handler function. Both have to return the same type - this is resulting type of 
the try expression. The exception handler gets called if the try-callback throws an exception. The argument 
will be the exception.

`gexcept.throw()` throws its argument as an exception. Throwing outside of a try-callback function is undefined behaviour.

```sh
gleam add gexcept
```
```gleam
import gexcept
import gleam/io

pub fn main() {
  gexcept.try(
    fn() {
      gexpect.throw("exception")

      "return value"
    },
    fn(exception) {
      "caught: " <> exception
    }
  )
  |> io.println
}
```

Further documentation can be found at <https://hexdocs.pm/gexcept>.

# Q and A

## Should I use this?
No. Never.

## Why you made this?
Because it is possible.

## Why shouldn't I use this?
It breaks with central design elements of gleam.

Building exceptions into your project
will cause you more problems than it will solve (if it solves any problems at all)

So please never ever use this package.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
