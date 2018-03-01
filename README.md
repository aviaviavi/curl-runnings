# curl-runnings

[![Build Status](https://travis-ci.org/aviaviavi/curl-runnings.svg?branch=master)](https://travis-ci.org/aviaviavi/curl-runnings)

Feel the rhythm! Feel the rhyme! Get on up, it's testing time! curl-runnings!

curl-runnings is a framework for writing declarative, curl based tests for your APIs. 

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

- clone this repo and run `stack install`
- download the releases 
- (soon) - `cabal install curl-runnings`

### Writing a test specification

A test spec is a top level list of test cases, each item represents a single curl and set of assertions about the response:

```yaml
---
# the top level of the file is an array of test cases
- name: test 1 # required
  url: http://your-endpoint.com # required
  requestMethod: GET # required
  # specify the json payload we expect here, if any
  expectData: # optional
    # `tag` refers to the type of matcher we want to use
    # valid tags are `Exactly` | `Contains`
    tag: Exactly
    # check for exactly this payload
    contents:
      okay: true
      msg: ''
  expectStatus: # requried
    # `tag` refers to the type of matcher we want to use
    # valid tags are `ExactCode` (contents :: number) | `AnyCodeIn` (contents :: [number])
    tag: ExactCode
    contents: 200
- name: test 2
  url: http://your-endpoint.com/path
  requestMethod: POST
  expectStatus:
    # Any code listed in `contents` is valid
    tag: AnyCodeIn
    contents:
    - 200
    - 201
  # json data to send with the request
  requestData:
    hello: there
    num: 1
- name: test 3
  url: http://your-url.com/other/path
  requestMethod: GET
  expectData:
    # apply a list of matchers to the returned json payload
    tag: Contains
    contents:
    # valid tags are `ValueMatch` | `KeyValueMatch`
    # find the value `true` anywhere in the payload. This can be useful for matching against values 
    # where you don't know the key ahead of time, or for values in a top level array.
    - tag: ValueMatch
      contents: true
    # `KeyValueMatch` looks for the key/value pair anywhere in the payload
    # here, {'okay': true} must be somewhere in the return payload
    - tag: KeyValueMatch
      matchKey: okay
      matchValue: true
  expectStatus:
    tag: ExactCode
    contents: 200
```

### Running

Once you've written a spec, simply run it with:

```bash
$ curl-runnings -f path/to/your/spec.yaml
```

If all your tests pass, curl-runnings will cleanly exit with a 0 code. A code of
1 will be returned if any tests failed.

For more info:

```bash
$ curl-runnings --help
```


### Future work

- [x] Json specifications for tests
- [x] Yaml specifications for tests
- [ ] Embedded dsl for specifications for tests. As the specification gets more complex.
  - [ ] Spec out dsl that can compile down into a yaml/json spec
  - [ ] Implement dsl
- [ ] More specification features
  - [ ] timeouts
  - [ ] retry logic
  - [ ] ability to configure alerts
