{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module LocationIQ.API
  -- * Client and Server
  ( ServerConfig(..)
  , LocationIQBackend
  , createLocationIQClient
  , runLocationIQServer
  , runLocationIQClient
  , runLocationIQClientWithManager
  , LocationIQClient
  -- ** Servant
  , LocationIQAPI
  ) where

import LocationIQ.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

-- | Servant type-level API, generated from the OpenAPI spec for LocationIQ.
type LocationIQAPI
    =    "balance.php" :> Verb 'GET 200 '[JSON] Balance -- 'balance' route
    :<|> "reverse.php" :> QueryParam "lat" Double :> QueryParam "lon" Double :> QueryParam "format" Text :> QueryParam "normalizecity" Int :> QueryParam "addressdetails" Int :> QueryParam "accept-language" Text :> QueryParam "namedetails" Int :> QueryParam "extratags" Int :> Verb 'GET 200 '[JSON] Location -- 'reverse' route
    :<|> "search.php" :> QueryParam "q" Text :> QueryParam "format" Text :> QueryParam "normalizecity" Int :> QueryParam "addressdetails" Int :> QueryParam "viewbox" Text :> QueryParam "bounded" Int :> QueryParam "limit" Int :> QueryParam "accept-language" Text :> QueryParam "countrycodes" Text :> QueryParam "namedetails" Int :> QueryParam "dedupe" Int :> QueryParam "extratags" Int :> Verb 'GET 200 '[JSON] [Location] -- 'search' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for LocationIQ.
-- The backend can be used both for the client and the server. The client generated from the LocationIQ OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createLocationIQClient@). Alternatively, provided
-- a backend, the API can be served using @runLocationIQServer@.
data LocationIQBackend m = LocationIQBackend
  { balance :: m Balance{- ^ The Balance API provides a count of request credits left in the user's account for the day. Balance is reset at midnight UTC everyday (00:00 UTC). -}
  , reverse :: Maybe Double -> Maybe Double -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Int -> m Location{- ^ Reverse geocoding is the process of converting a coordinate or location (latitude, longitude) to a readable address or place name. This permits the identification of nearby street addresses, places, and/or area subdivisions such as a neighborhood, county, state, or country. -}
  , search :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> m [Location]{- ^ The Search API allows converting addresses, such as a street address, into geographic coordinates (latitude and longitude). These coordinates can serve various use-cases, from placing markers on a map to helping algorithms determine nearby bus stops. This process is also known as Forward Geocoding. -}
  }

newtype LocationIQClient a = LocationIQClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative LocationIQClient where
  pure x = LocationIQClient (\_ _ -> pure x)
  (LocationIQClient f) <*> (LocationIQClient x) =
    LocationIQClient (\manager url -> f manager url <*> x manager url)

instance Monad LocationIQClient where
  (LocationIQClient a) >>= f =
    LocationIQClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO LocationIQClient where
  liftIO io = LocationIQClient (\_ _ -> liftIO io)

createLocationIQClient :: LocationIQBackend LocationIQClient
createLocationIQClient = LocationIQBackend{..}
  where
    ((coerce -> balance) :<|>
     (coerce -> reverse) :<|>
     (coerce -> search)) = client (Proxy :: Proxy LocationIQAPI)

-- | Run requests in the LocationIQClient monad.
runLocationIQClient :: ServerConfig -> LocationIQClient a -> ExceptT ServantError IO a
runLocationIQClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runLocationIQClientWithManager manager clientConfig cl

-- | Run requests in the LocationIQClient monad using a custom manager.
runLocationIQClientWithManager :: Manager -> ServerConfig -> LocationIQClient a -> ExceptT ServantError IO a
runLocationIQClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the LocationIQ server at the provided host and port.
runLocationIQServer :: MonadIO m => ServerConfig -> LocationIQBackend (ExceptT ServantErr IO)  -> m ()
runLocationIQServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy LocationIQAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend LocationIQBackend{..} =
      (coerce balance :<|>
       coerce reverse :<|>
       coerce search)
