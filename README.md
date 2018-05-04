# curl-runnings

[![Build Status](https://travis-ci.org/aviaviavi/curl-runnings.svg?branch=master)](https://travis-ci.org/aviaviavi/curl-runnings) [![Hackage](https://img.shields.io/hackage/v/curl-runnings.svg)](https://hackage.haskell.org/package/curl-runnings)



_Feel the rhythm! Feel the rhyme! Get on up, it's testing time! curl-runnings!_

curl-runnings is a framework for writing declarative, curl based tests for your
APIs. Write your tests quickly and correctly with a straight-forward
specification in yaml or json that can encode simple but powerful matchers
against responses.

Alternatively, you can use the curl-runnings library to write your tests in
Haskell (though a haskell setup is absolutely not required to use this tool).

### Why?

When writing curl based smoke/integration tests for APIs using bash and `curl`
is very convenient, but quickly becomes hard to maintain. Writing matchers for
json output quickly becomes unweildy and error prone. Writing these sorts of
tests in a more traditional programming language is fine, but certainly more
time consuming to write than some simple curl requests. curl-runnings aims to
make it very easy to write tests that just hit some endpoints and verify the
output looks sane.

Now you can write your tests just as data in a yaml or json file,
and curl-runnings will take care of the rest!

While yaml/json is the current way to write curl-runnings tests, this project is
being built in a way that should lend itself well to an embedded domain specific
language, which is a future goal for the project. curl-runnings specs in Dhall
is also being developed and may fufill the same needs.

### Installing

There are few options to install:

- download the releases from the
  github [releases page](https://github.com/aviaviavi/curl-runnings/releases)
- install the binary with `stack` or `cabal`
- build from source with `stack`

### Writing a test specification

Write your tests specs in a yaml or json file. See /examples to get
started. A test spec is a top level array of test cases, each item represents a
single curl and set of assertions about the response.

### Running

Once you've written a spec, simply run it with:

```bash $ curl-runnings -f path/to/your/spec.yaml ```

If all your tests pass, curl-runnings will cleanly exit with a 0 code. A code of
1 will be returned if any tests failed.

For more info:

```bash $ curl-runnings --help ```

### Contributing

Curl-runnings is totally usable now but is also being actively developed.
Contributions in any form are welcome and encouraged. Don't be shy! :D

### Roadmap

- [x] Json specifications for tests
- [x] Yaml specifications for tests
- [ ] Dhall specifications for tests
- [ ] More specification features
  - [x] Reference values from previous json responses in matchers
  - [x] Environment variable interpolation
  - [ ] Call out to arbitrary shell commands in and between test cases
  - [ ] Timeouts
  - [ ] Support for non-json content type
  - [ ] Retry logic
- [ ] A DSL for writing test specs
  
