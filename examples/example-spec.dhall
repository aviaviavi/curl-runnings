-- Your curl-runnings specs can be written in dhall, which can give you great
-- type safety and interpolation abilities.

let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

let CurlRunnings = ./../dhall/curl-runnings.dhall

let List/map = https://prelude.dhall-lang.org/List/map

let Optional/map = https://prelude.dhall-lang.org/Optional/map

let Map = https://prelude.dhall-lang.org/Map/Type

let host = "https://tabdextension.com"

in    List/map
        CurlRunnings.Case.Type
        CurlRunnings.HydratedCase.Type
        CurlRunnings.hydrateCase
        [ CurlRunnings.Case::{
          , expectData = Some
              ( CurlRunnings.ExpectData.Exactly
                  ( JSON.object
                      [ { mapKey = "ping", mapValue = JSON.string "pong" } ]
                  )
              )
          , expectStatus = 200
          , name = "test 1"
          , requestMethod = CurlRunnings.HttpMethod.GET
          , url = host ++ "/ping"
          }
        , CurlRunnings.Case::{
          , expectData = Some
              ( CurlRunnings.ExpectData.Contains
                  [ CurlRunnings.PartialMatcher.KeyMatch "ping"
                  , CurlRunnings.PartialMatcher.ValueMatch (JSON.string "pong")
                  , CurlRunnings.PartialMatcher.KeyValueMatch
                      { key = "ping", value = JSON.string "pong" }
                  ]
              )
          , expectStatus = 200
          , name = "test 2"
          , requestMethod = CurlRunnings.HttpMethod.GET
          , url = host ++ "/ping"
          }
        , CurlRunnings.Case::{
          , expectData = Some
              ( CurlRunnings.ExpectData.NotContains
                  [ CurlRunnings.PartialMatcher.KeyMatch "poing"
                  , CurlRunnings.PartialMatcher.ValueMatch (JSON.string "pongg")
                  , CurlRunnings.PartialMatcher.KeyValueMatch
                      { key = "ping", value = JSON.string "poong" }
                  ]
              )
          , expectStatus = 200
          , name = "test 3"
          , requestMethod = CurlRunnings.HttpMethod.GET
          , url = host ++ "/ping"
          }
        , CurlRunnings.Case::{
          , expectData = Some
              ( CurlRunnings.ExpectData.MixedContains
                  { contains =
                    [ CurlRunnings.PartialMatcher.KeyMatch "ping"
                    , CurlRunnings.PartialMatcher.ValueMatch
                        (JSON.string "\$<RESPONSES[-1].ping>")
                    , CurlRunnings.PartialMatcher.KeyValueMatch
                        { key = "ping", value = JSON.string "pong" }
                    ]
                  , notContains =
                    [ CurlRunnings.PartialMatcher.KeyMatch "poing"
                    , CurlRunnings.PartialMatcher.ValueMatch
                        (JSON.string "pongg")
                    , CurlRunnings.PartialMatcher.KeyValueMatch
                        { key = "ping", value = JSON.string "poong" }
                    ]
                  }
              )
          , expectStatus = 200
          , name = "test 4"
          , requestMethod = CurlRunnings.HttpMethod.GET
          , url = host ++ "/ping"
          }
        , CurlRunnings.Case::{
          , expectStatus = 405
          , name = "test 5"
          , requestMethod = CurlRunnings.HttpMethod.POST
          , url = host ++ "/ping"
          , queryParameters = [ { mapKey = "test", mapValue = "asdf" } ]
          , requestData = Some
              ( CurlRunnings.RequestData.JSON
                  ( JSON.object
                      [ { mapKey = "key", mapValue = JSON.string "value" } ]
                  )
              )
          }
        ]
    : List CurlRunnings.HydratedCase.Type
