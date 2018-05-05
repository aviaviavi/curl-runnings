# curl-runnings

[![Build Status](https://travis-ci.org/aviaviavi/curl-runnings.svg?branch=master)](https://travis-ci.org/aviaviavi/curl-runnings) [![Hackage](https://img.shields.io/hackage/v/curl-runnings.svg)](https://hackage.haskell.org/package/curl-runnings)

_Feel the rhythm! Feel the rhyme! Get on up, it's testing time! curl-runnings!_

curl-runnings is a framework for writing declarative, curl based tests for your
APIs. Write your tests quickly and correctly with a straight-forward
specification in yaml or json that can encode simple but powerful matchers
against responses.

Alternatively, you can use the curl-runnings library to write your tests in
Haskell (though a Haskell setup is absolutely not required to use this tool).

### Why?

This library came out of a pain-point my coworkers at
[DotDashPay](https://dotdashpay.com) and I were running into during development:
Writing integration tests for our APIs was generally annoying. They were time
consuming to write especially considering how basic they were, and we are a
small startup where developer time is in short supply. Over time, we found
ourselves at sometimes just writing bash scripts that would `curl` our various
endpoints and check the output with very basic matchers. These tests were fast
to write, but quickly became difficult to maintain as complexity was added. Not
only did maintence become challenging, but the whole system was very error prone
and confidence in the tests overall was decreasing. At the end of the day, we
needed to just curl some endpoints and verify the output looks sane, and do this
quickly and correctly. This is precisely the goal of curl-runnings.

Now you can write your tests just as data in a yaml or json file,
and curl-runnings will take care of the rest!

While yaml/json is the current way to write curl-runnings tests, this project is
being built in a way that should lend itself well to an embedded domain specific
language, which is a future goal for the project. curl-runnings specs in Dhall
is also being developed and may fulfill the same needs.

### Installing

There are few options to install:

- download the linux-built releases from the
  github [releases page](https://github.com/aviaviavi/curl-runnings/releases)
- install the binary with `stack` or `cabal`
- build from source with `stack`

### Writing a test specification

Curl runnings tests are just data! A test spec is a top level array of test
cases, where each item represents a single curl and set of assertions about the
response. Write your tests specs in a yaml or json file.

```yaml
---
# example-test.yaml
#
# the top level of the file is an array of test cases
- name: A curl runnings test case
  url: http://your-endpoint.com/status
  requestMethod: GET
  # Specify the json payload we expect here
  expectData:
    # The 1 key in this object specifies the matcher we want
    # to use to test the returned payload. In this case, we
    # require the payload is exactly what we specify.
    exactly:
      okay: true
      msg: 'a message'
  # Assertions about the returned status code. Pass in
  # an acceptable code or list of codes
  expectStatus: 200

```

See /examples for more example curl runnings specifications, which walk
through some of the other features that can be encoded in your tests.

### Running

Once you've written a spec, simply run it with:

```curl-runnings -f path/to/your/spec.yaml ```

If all your tests pass, curl-runnings will cleanly exit with a 0 code. A code of
1 will be returned if any tests failed.

For more info:

```curl-runnings --help ```

### Contributing

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
  
