# Auto-Generated OpenAPI Bindings to `LocationIQ`

The library in `lib` provides auto-generated-from-OpenAPI bindings to the LocationIQ API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

Otherwise, if you already have a Stack project, you can include this package under the `packages` key in your `stack.yaml`:
```yaml
packages:
- location:
    git: https://github.com/yourGitOrg/yourGitRepo
    commit: somecommit
```

## Main Interface

The main interface to this library is in the `LocationIQ.API` module, which exports the LocationIQBackend type. The LocationIQBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createLocationIQClient` function, which will generate a function for every endpoint of the API.
Then these functions can be invoked with `runLocationIQClientWithManager` or more conveniently with `callLocationIQClient`
(depending if you want an `Either` back or you want to catch) to access the API endpoint they refer to, if the API is served
at the `url` you specified.

For example, if `localhost:8080` is serving the LocationIQ API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import LocationIQ.API as API

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)


main :: IO ()
main = do
  -- Configure the BaseUrl for the client
  url <- parseBaseUrl "http://localhost:8080/"

  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- newManager tlsManagerSettings

  -- Create the client (all endpoint functions will be available)
  LocationIQBackend{..} <- API.createLocationIQClient

  -- Any LocationIQ API call can go here, e.g. here we call `getSomeEndpoint`
  API.callLocationIQ (mkClientEnv manager url) getSomeEndpoint
```

## Creating a Server

In order to create a server, you must use the `runLocationIQMiddlewareServer` function. However, you unlike the client, in which case you *got* a `LocationIQBackend`
from the library, you must instead *provide* a `LocationIQBackend`. For example, if you have defined handler functions for all the
functions in `LocationIQ.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import LocationIQ.API
-- required dependency: wai
import Network.Wai (Middleware)
-- required dependency: wai-extra
import Network.Wai.Middleware.RequestLogger (logStdout)

-- A module you wrote yourself, containing all handlers needed for the LocationIQBackend type.
import LocationIQ.Handlers

-- If you would like to not use any middlewares you could use runLocationIQServer instead

-- Combined middlewares
requestMiddlewares :: Middleware
requestMiddlewares = logStdout

-- Run a LocationIQ server on localhost:8080
main :: IO ()
main = do
  let server = LocationIQBackend{..}
      config = Config "http://localhost:8080/"
  runLocationIQMiddlewareServer config requestMiddlewares server
```
