-- Your curl-runnings specs can be written in dhall, which can give you great
-- type safety and interpolation abilities.
let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

let List/map = https://prelude.dhall-lang.org/List/map

let HttpMethod
    : Type
    = < GET | POST | PUT | PATCH | DELETE >

let PartialMatcher =
      < KeyMatch : Text
      | ValueMatch : JSON.Type
      | KeyValueMatch : { key : Text, value : JSON.Type }
      >

let ExpectData =
      < Exactly : JSON.Type
      | Contains : List PartialMatcher
      | NotContains : List PartialMatcher
      | MixedContains :
          { contains : List PartialMatcher, notContains : List PartialMatcher }
      >

let KeyValMatchJSON =
      { keyMatch : Optional PartialMatcher
      , valueMatch : Optional PartialMatcher
      , keyValueMatch : Optional PartialMatcher
      }

let makeContains =
      λ(containsMatcher : PartialMatcher) →
        merge
          { KeyMatch =
              λ(k : Text) →
                { keyMatch = Some (PartialMatcher.KeyMatch k)
                , valueMatch = None PartialMatcher
                , keyValueMatch = None PartialMatcher
                }
          , ValueMatch =
              λ(v : JSON.Type) →
                { keyMatch = None PartialMatcher
                , valueMatch = Some (PartialMatcher.ValueMatch v)
                , keyValueMatch = None PartialMatcher
                }
          , KeyValueMatch =
              λ(args : { key : Text, value : JSON.Type }) →
                { keyMatch = None PartialMatcher
                , valueMatch = None PartialMatcher
                , keyValueMatch = Some
                    ( PartialMatcher.KeyValueMatch
                        { key = args.key, value = args.value }
                    )
                }
          }
          containsMatcher

let makeExpect =
      λ(matcher : ExpectData) →
        merge
          { Exactly =
              λ(j : JSON.Type) →
                { exactly = Some (ExpectData.Exactly j)
                , contains = None (List KeyValMatchJSON)
                , notContains = None (List KeyValMatchJSON)
                }
          , Contains =
              λ(ms : List PartialMatcher) →
                { exactly = None ExpectData
                , contains = Some
                    (List/map PartialMatcher KeyValMatchJSON makeContains ms)
                , notContains = None (List KeyValMatchJSON)
                }
          , NotContains =
              λ(ms : List PartialMatcher) →
                { exactly = None ExpectData
                , contains = None (List KeyValMatchJSON)
                , notContains = Some
                    (List/map PartialMatcher KeyValMatchJSON makeContains ms)
                }
          , MixedContains =
              λ ( args
                : { contains : List PartialMatcher
                  , notContains : List PartialMatcher
                  }
                ) →
                { exactly = None ExpectData
                , contains = Some
                    ( List/map
                        PartialMatcher
                        KeyValMatchJSON
                        makeContains
                        args.contains
                    )
                , notContains = Some
                    ( List/map
                        PartialMatcher
                        KeyValMatchJSON
                        makeContains
                        args.notContains
                    )
                }
          }
          matcher

let host = "https://tabdextension.com"

in  [ { expectData =
          makeExpect
            ( ExpectData.Exactly
                ( JSON.object
                    [ { mapKey = "ping", mapValue = JSON.string "pong" } ]
                )
            )
      , expectStatus = +200
      , name = "test 1"
      , requestMethod = HttpMethod.GET
      , url = host ++ "/ping"
      }
    , { expectData =
          makeExpect
            ( ExpectData.Contains
                [ PartialMatcher.KeyMatch "ping"
                , PartialMatcher.ValueMatch (JSON.string "pong")
                , PartialMatcher.KeyValueMatch
                    { key = "ping", value = JSON.string "pong" }
                ]
            )
      , expectStatus = +200
      , name = "test 2"
      , requestMethod = HttpMethod.GET
      , url = host ++ "/ping"
      }
    , { expectData =
          makeExpect
            ( ExpectData.NotContains
                [ PartialMatcher.KeyMatch "poing"
                , PartialMatcher.ValueMatch (JSON.string "pongg")
                , PartialMatcher.KeyValueMatch
                    { key = "ping", value = JSON.string "poong" }
                ]
            )
      , expectStatus = +200
      , name = "test 3"
      , requestMethod = HttpMethod.GET
      , url = host ++ "/ping"
      }
    , { expectData =
          makeExpect
            ( ExpectData.MixedContains
                { contains =
                  [ PartialMatcher.KeyMatch "ping"
                  , PartialMatcher.ValueMatch (JSON.string "pong")
                  , PartialMatcher.KeyValueMatch
                      { key = "ping", value = JSON.string "pong" }
                  ]
                , notContains =
                  [ PartialMatcher.KeyMatch "poing"
                  , PartialMatcher.ValueMatch (JSON.string "pongg")
                  , PartialMatcher.KeyValueMatch
                      { key = "ping", value = JSON.string "poong" }
                  ]
                }
            )
      , expectStatus = +200
      , name = "test 4"
      , requestMethod = HttpMethod.GET
      , url = host ++ "/ping"
      }
    ]
