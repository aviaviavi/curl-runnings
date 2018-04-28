-- Your curl-runnings specs can be written in dhall, which can give you great
-- type safety and interpolation abilities.
    let HttpMethod/Get =
          < HttpMethod/Get    = "get"
          | HttpMethod/Delete : {}
          | HttpMethod/Patch  : {}
          | HttpMethod/Post   : {}
          | HttpMethod/Put    : {}
          >

in  let HttpMethod/Delete =
          < HttpMethod/Delete = "delete"
          | HttpMethod/Get    : {}
          | HttpMethod/Patch  : {}
          | HttpMethod/Post   : {}
          | HttpMethod/Put    : {}
          >

in  let HttpMethod/Patch =
          < HttpMethod/Patch  = "patch"
          | HttpMethod/Delete : {}
          | HttpMethod/Get    : {}
          | HttpMethod/Post   : {}
          | HttpMethod/Put    : {}
          >

in  let HttpMethod/Post =
          < HttpMethod/Post   = "post"
          | HttpMethod/Delete : {}
          | HttpMethod/Get    : {}
          | HttpMethod/Patch  : {}
          | HttpMethod/Put    : {}
          >

in  let HttpMethod/Put =
          < HttpMethod/Put    = "put"
          | HttpMethod/Delete : {}
          | HttpMethod/Get    : {}
          | HttpMethod/Patch  : {}
          | HttpMethod/Post   : {}
          >

in  let host = "https://tabdextension.com"

in  [ { expectData    = { exactly = { ping = "$<SUITE[-1].ping>" } }
      , expectStatus  = +200
      , name          = "test 2"
      , requestMethod = HttpMethod/Get
      , url           = host ++ "/ping"
      }
    ]
