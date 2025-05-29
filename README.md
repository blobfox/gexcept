# gexcept

[![Package Version](https://img.shields.io/hexpm/v/gexcept)](https://hex.pm/packages/gexcept)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gexcept/)

Ever wanted to use exceptions in Gleam? Now you can!

`gexcept` is a gleam library that provides exception handling in Gleam. It is build to be as generic as
possible as to not interfer with any existing type systems. It works for both Erlang and JavaScript targets.

## Installation

To add `gexecpt` to your project, use the following command.

```sh
gleam add gexcept
```

## Usage

The library provides two primary functions:

- `throw()` to throw an exception, and
- `try()` for catching and handling them.

Additionally, it includes functions for dealing with `Result` and `Option` instances. For more details, please 
refer to the documentation.

Here's a basic example - you can find some more in `test/gexcept_test.gleam`.

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

## Q&A

### Should I use this?
![steve carell noo meme gif](https://github.com/blobfox/gexcept/raw/main/assets/steve-carell-nooo.gif?raw=true)

### Why did you make this?
Because it is possible.

### Why shouldn't I use this?
It undermines the core design principles of Gleam.

Building exceptions into your project will cause you more problems than it will solve (if it solves any problems 
at all).

So, please, never ever use this package.

## Further Notes

The exceptions themselves are not type-checked, and there is no easy way to achive that.

Calling `throw()` without a surrounding `try()`-call will probably crash the program, but I consider this
undefined behaviour.

## Development

You can run the test suite using these commands.

```sh
gleam test --target=erlang
gleam test --target=javascript
```
