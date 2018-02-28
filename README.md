# curl-runnings

A framework for writing declarative curl based tests.

### Why?

When writing curl based smoke/integration tests for APIs using bash and `curl`
is very convenient, but quickly becomes hard to maintain. Writing matchers for
json output quickly becomes unweildy and error prone. Writing these sorts of
tests in a more traditional programming language is fine, but certainly more
time consuming to write than some simple curl requests. curl-runnings aims to
make it very simple to write tests that curl some endpoints and verify the
output looks sane.

### Installing

- clone this repo and run `stack install`
- download the releases 
- (soon) - `cabal install curl-runnings`
