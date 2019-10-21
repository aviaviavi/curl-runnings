# curl-runnings

[![Build Status](https://travis-ci.org/aviaviavi/curl-runnings.svg?branch=master)](https://travis-ci.org/aviaviavi/curl-runnings) 
[![Hackage](https://img.shields.io/hackage/v/curl-runnings.svg)](https://hackage.haskell.org/package/curl-runnings)
[![Scarf](https://scarf.sh/package/badge/curl-runnings)](https://scarf.sh/package/avi/curl-runnings)

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
ourselves sometimes just writing bash scripts that would `curl` our various
endpoints and check the output with very basic matchers. These tests were fast
to write, but quickly became difficult to maintain as complexity was added. Not
only did maintenance become challenging, but the whole system was very error prone
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

The best way to install curl-runnings is with the [scarf](https://scarf.sh)
package manager.

```bash
# If you don't have scarf, you can easily install it with:
$ curl -L https://scarf.sh/install | bash
 
$ scarf install curl-runnings
```

Alternatively, you can compile from source with stack.

### Writing a test specification

Curl runnings tests are just data! A test spec is an object containing an array
of `cases`, where each item represents a single curl and set of assertions about
the response. Write your tests specs in a yaml or json file. Note: the legacy
format of a top level array of test cases is still supported, but may not be in
future releases.


```yaml
---
# example-test.yaml
#
# specify all your test cases as an array keys on `cases`
cases:
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
through some of the other features that can be encoded in your tests such as:
- reference data from previous responses of previous test cases
- reference environment variables
- various easy-to-use json matchers
- support for importing data from other yaml files in your spec

### Running

Once you've written a spec, simply run it with:

```curl-runnings -f path/to/your/spec.yaml ```

(hint: try using the --verbose flag for more output)

If all your tests pass, curl-runnings will cleanly exit with a 0 code. A code of
1 will be returned if any tests failed.

You can also select specific test cases by filtering via regex by using the
`--grep` flag. Just make sure your case isn't referencing data from previous
examples that won't get run!

For more info:

```curl-runnings --help ```


### Running With Docker
A dockerfile is included in the root of the project. The Dockerfile will expect the linux based curl-runnings executable in the same directory as the Dockerfile and a `tests.yml` file. You can download the latest executable from the release page : https://github.com/aviaviavi/curl-runnings/releases .

``` docker build . -t curl-runnings-tests```

```  docker run curl-runnings-tests```

If you use docker-compose, you can add this to docker-compose.yml:

```
tests:
    build:
      context: .
      dockerfile: ./Dockerfile
```


### Contributing

Contributions in any form are welcome and encouraged. Don't be shy! :D

