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

let KeyValMatchHydrated =
      { keyMatch : Optional PartialMatcher
      , valueMatch : Optional PartialMatcher
      , keyValueMatch : Optional PartialMatcher
      }

let ExpectHeaders =
      < HeaderString : Text
      | HeaderKeyVal : { key : Optional Text, value : Optional Text }
      >

let hydrateContains =
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

let ExpectResponseHydrated =
      { exactly : Optional ExpectData
      , contains : Optional (List KeyValMatchHydrated)
      , notContains : Optional (List KeyValMatchHydrated)
      }

let hydrateExpectData =
      λ(matcher : ExpectData) →
        merge
          { Exactly =
              λ(j : JSON.Type) →
                { exactly = Some (ExpectData.Exactly j)
                , contains = None (List KeyValMatchHydrated)
                , notContains = None (List KeyValMatchHydrated)
                }
          , Contains =
              λ(ms : List PartialMatcher) →
                { exactly = None ExpectData
                , contains = Some
                    ( List/map
                        PartialMatcher
                        KeyValMatchHydrated
                        hydrateContains
                        ms
                    )
                , notContains = None (List KeyValMatchHydrated)
                }
          , NotContains =
              λ(ms : List PartialMatcher) →
                { exactly = None ExpectData
                , contains = None (List KeyValMatchHydrated)
                , notContains = Some
                    ( List/map
                        PartialMatcher
                        KeyValMatchHydrated
                        hydrateContains
                        ms
                    )
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
                        KeyValMatchHydrated
                        hydrateContains
                        args.contains
                    )
                , notContains = Some
                    ( List/map
                        PartialMatcher
                        KeyValMatchHydrated
                        hydrateContains
                        args.notContains
                    )
                }
          }
          matcher

let BodyType = < json | urlencoded >

let RequestData = < JSON : JSON.Type | UrlEncoded : Map Text Text >

let Auth = < BasicAuth : { username : Text, password : Text } >

let AuthHydrated = { basic : Optional { username : Text, password : Text } }

let hydrateAuth =
      λ(auth : Auth) →
        merge
          { BasicAuth =
              λ(args : { username : Text, password : Text }) →
                { basic = Some
                  { username = args.username, password = args.password }
                }
          }
          auth

let RequestDataHydrated = { bodyType : BodyType, content : RequestData }

let hydrateRequestData =
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
          , auth : Optional Auth
          }
      , default =
        { expectData = None ExpectData
        , headers = None Text
        , expectHeaders = None (List ExpectHeaders)
        , allowedRedirects = 10
        , queryParameters = [] : Map Text Text
        , requestData = None RequestData
        , auth = None Auth
        }
      }

let HydratedCase =
      { Type =
          { name : Text
          , url : Text
          , requestMethod : HttpMethod
          , queryParameters : JSON.Type
          , expectData : Optional ExpectResponseHydrated
          , expectStatus : Natural
          , headers : Optional Text
          , expectHeaders : Optional (List ExpectHeaders)
          , allowedRedirects : Natural
          , requestData : Optional RequestDataHydrated
          , auth : Optional AuthHydrated
          }
      , default =
        { expectData = None ExpectResponseHydrated
        , headers = None Text
        , expectHeaders = None (List ExpectHeaders)
        , allowedRedirects = 10
        , queryParameters = JSON.null
        , requestData = None RequestDataHydrated
        , auth = None AuthHydrated
        }
      }

let hydrateCase =
      λ(c : Case.Type) →
          c
        ⫽ { queryParameters = makeQueryParams c.queryParameters
          , expectData =
              Optional/map
                ExpectData
                ExpectResponseHydrated
                hydrateExpectData
                c.expectData
          , requestData =
              Optional/map
                RequestData
                RequestDataHydrated
                hydrateRequestData
                c.requestData
          , auth = Optional/map Auth AuthHydrated hydrateAuth c.auth
          }

let hydrateCases =
      λ(cases : List Case.Type) →
        List/map Case.Type HydratedCase.Type hydrateCase cases

in  { Case
    , HydratedCase
    , hydrateCase
    , hydrateCases
    , HttpMethod
    , ExpectData
    , PartialMatcher
    , ExpectHeaders
    , RequestData
    , Auth
    , AuthHydrated
    , hydrateAuth
    }
