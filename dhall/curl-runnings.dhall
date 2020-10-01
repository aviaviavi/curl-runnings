let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

let List/map = https://prelude.dhall-lang.org/List/map

let Optional/map = https://prelude.dhall-lang.org/Optional/map

let Map = https://prelude.dhall-lang.org/Map/Type

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

let ExpectHeaders =
      < HeaderString : Text
      | HeaderKeyVal : { key : Optional Text, value : Optional Text }
      >

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

let ExpectResponseJSON =
      { exactly : Optional ExpectData
      , contains : Optional (List KeyValMatchJSON)
      , notContains : Optional (List KeyValMatchJSON)
      }

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

let BodyType = < json | urlencoded >

let RequestData = < JSON : JSON.Type | UrlEncoded : Map Text Text >

let RequestDataJSON = { bodyType : BodyType, content : RequestData }

let makeRequestDataJSON =
      λ(reqData : RequestData) →
        merge
          { JSON =
              λ(json : JSON.Type) →
                { bodyType = BodyType.json, content = RequestData.JSON json }
          , UrlEncoded =
              λ(encoded : Map Text Text) →
                { bodyType = BodyType.urlencoded
                , content = RequestData.UrlEncoded encoded
                }
          }
          reqData

let makeQueryParams =
      λ(params : Map Text Text) →
        JSON.object
          ( List/map
              { mapKey : Text, mapValue : Text }
              { mapKey : Text, mapValue : JSON.Type }
              ( λ(args : { mapKey : Text, mapValue : Text }) →
                  { mapKey = args.mapKey, mapValue = JSON.string args.mapValue }
              )
              params
          )

let QueryParams = List { mapKey : Text, mapValue : JSON.Type }

let Case =
      { Type =
          { name : Text
          , url : Text
          , requestMethod : HttpMethod
          , queryParameters : Map Text Text
          , expectData : Optional ExpectData
          , expectStatus : Natural
          , headers : Optional Text
          , expectHeaders : Optional (List ExpectHeaders)
          , allowedRedirects : Natural
          , requestData : Optional RequestData
          }
      , default =
        { expectData = None ExpectData
        , headers = None Text
        , expectHeaders = None (List ExpectHeaders)
        , allowedRedirects = 10
        , queryParameters = [] : Map Text Text
        , requestData = None RequestData
        }
      }

let HydratedCase =
      { Type =
          { name : Text
          , url : Text
          , requestMethod : HttpMethod
          , queryParameters : JSON.Type
          , expectData : Optional ExpectResponseJSON
          , expectStatus : Natural
          , headers : Optional Text
          , expectHeaders : Optional (List ExpectHeaders)
          , allowedRedirects : Natural
          , requestData : Optional RequestDataJSON
          }
      , default =
        { expectData = None ExpectResponseJSON
        , headers = None Text
        , expectHeaders = None (List ExpectHeaders)
        , allowedRedirects = 10
        , queryParameters = JSON.null
        , requestData = None RequestDataJSON
        }
      }

let hydrateCase =
      λ(c : Case.Type) →
          c
        ⫽ { queryParameters = makeQueryParams c.queryParameters
          , expectData =
              Optional/map ExpectData ExpectResponseJSON makeExpect c.expectData
          , requestData =
              Optional/map
                RequestData
                RequestDataJSON
                makeRequestDataJSON
                c.requestData
          }

in  { Case
    , HydratedCase
    , hydrateCase
    , HttpMethod
    , ExpectData
    , PartialMatcher
    , KeyValMatchJSON
    , ExpectHeaders
    , RequestData
    }
