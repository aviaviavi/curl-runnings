# curl-runnings

[![Build Status](https://travis-ci.org/aviaviavi/curl-runnings.svg?branch=master)](https://travis-ci.org/aviaviavi/curl-runnings)

_Feel the rhythm! Feel the rhyme! Get on up, it's testing time! curl-runnings!_

curl-runnings is a framework for writing declarative, curl based tests for your APIs. 

Write your tests quickly and correctly with a straight-forward specification in
yaml or json. A DSL for writing your tests is on the way! Alternatively, you can
use the curl-runnings library to write your tests directly in Haskell.

### Why?

When writing curl based smoke/integration tests for APIs using bash and `curl`
is very convenient, but quickly becomes hard to maintain. Writing matchers for
json output quickly becomes unweildy and error prone. Writing these sorts of
tests in a more traditional programming language is fine, but certainly more
time consuming to write than some simple curl requests. curl-runnings aims to
make it very simple to write tests that curl some endpoints and verify the
output looks sane.

With curl-runnings, you can write your tests just as data in a yaml or json file,
curl runnings will take care of the rest!

While yaml/json is the current way to write curl-runnings tests, this project is
being built in a way that should lend itself well to an embedded domain specific
language, which is a future goal for the project

### Installing

There are few options to install:

- download the releases from the
  github [releases page](https://github.com/aviaviavi/curl-runnings/releases)
- `stack install curl-runnings`
- `cabal install curl-runnings`
- build from source with `stack`

### Writing a test specification

For now, you write your tests specs in a yaml or json file. See /examples to get
started. A test spec is a top level array of test cases, each item represents a
single curl and set of assertions about the response.

### Running

Once you've written a spec, simply run it with:

```bash $ curl-runnings -f path/to/your/spec.yaml ```

If all your tests pass, curl-runnings will cleanly exit with a 0 code. A code of
1 will be returned if any tests failed.

For more info:

```bash $ curl-runnings --help ```

### Roadmap

Curl-runnings is totally usable now but is also being actively developed.
Contributions in any form are welcome and encouraged. Don't be shy! :D

- [x] Json specifications for tests
- [x] Yaml specifications for tests
- [ ] More specification features
  - [x] Reference values from previous json responses in matchers
  - [x] Environment variable interpolation
  - [ ] Call out to arbitrary shell commands in and between test cases
  - [ ] Verbosity levels: print out the curl commands being run
  - [ ] Timeouts
  - [ ] Support for non-json content type
  - [ ] Retry logic
  - [ ] Ability to configure alerts
- [ ] Embedded dsl for specifications for tests. As the specification gets more complex.
  - [ ] Spec out dsl that can compile down into a yaml/json spec
  - [ ] Implement dsl
  
