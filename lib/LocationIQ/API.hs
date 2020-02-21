{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module LocationIQ.API
  -- * Client and Server
  ( Config(..)
  , LocationIQBackend(..)
  , createLocationIQClient
  , runLocationIQServer
  , runLocationIQMiddlewareServer
  , runLocationIQClient
  , runLocationIQClientWithManager
  , callLocationIQ
  , LocationIQClient
  , LocationIQClientError(..)
  -- ** Servant
  , LocationIQAPI
  ) where

import           LocationIQ.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




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


-- | Servant type-level API, generated from the OpenAPI spec for LocationIQ.
type LocationIQAPI
    =    "autocomplete.php" :> QueryParam "q" Text :> QueryParam "limit" Int :> QueryParam "viewbox" Text :> QueryParam "bounded" Int :> QueryParam "countrycodes" Text :> QueryParam "normalizecity" Int :> QueryParam "accept-language" Text :> QueryParam "tag" Text :> Verb 'GET 200 '[JSON] [Value] -- 'autocomplete' route
    :<|> "balance.php" :> Verb 'GET 200 '[JSON] Balance -- 'balance' route
    :<|> "directions" :> "driving" :> Capture "coordinates" Text :> QueryParam "bearings" Text :> QueryParam "radiuses" Text :> QueryParam "generate_hints" Text :> QueryParam "approaches" Text :> QueryParam "exclude" Text :> QueryParam "alternatives" Double :> QueryParam "steps" Text :> QueryParam "annotations" Text :> QueryParam "geometries" Text :> QueryParam "overview" Text :> QueryParam "continue_straight" Text :> Verb 'GET 200 '[JSON] DirectionsDirections -- 'directions' route
    :<|> "matching" :> "driving" :> Capture "coordinates" Text :> QueryParam "generate_hints" Text :> QueryParam "approaches" Text :> QueryParam "exclude" Text :> QueryParam "bearings" Text :> QueryParam "radiuses" Text :> QueryParam "steps" Text :> QueryParam "annotations" Text :> QueryParam "geometries" Text :> QueryParam "overview" Text :> QueryParam "timestamps" Text :> QueryParam "gaps" Text :> QueryParam "tidy" Text :> QueryParam "waypoints" Text :> Verb 'GET 200 '[JSON] DirectionsMatching -- 'matching' route
    :<|> "matrix" :> "driving" :> Capture "coordinates" Text :> QueryParam "bearings" Text :> QueryParam "radiuses" Text :> QueryParam "generate_hints" Text :> QueryParam "approaches" Text :> QueryParam "exclude" Text :> QueryParam "annotations" Text :> QueryParam "sources" Int :> QueryParam "destinations" Int :> QueryParam "fallback_speed" Double :> QueryParam "fallback_coordinate" Text :> Verb 'GET 200 '[JSON] DirectionsMatrix -- 'matrix' route
    :<|> "nearest" :> "driving" :> Capture "coordinates" Text :> QueryParam "generate_hints" Text :> QueryParam "exclude" Text :> QueryParam "bearings" Text :> QueryParam "radiuses" Text :> QueryParam "approaches" Text :> QueryParam "number" Int :> Verb 'GET 200 '[JSON] DirectionsNearest -- 'nearest' route
    :<|> "reverse.php" :> QueryParam "lat" Double :> QueryParam "lon" Double :> QueryParam "format" Text :> QueryParam "normalizecity" Int :> QueryParam "addressdetails" Int :> QueryParam "accept-language" Text :> QueryParam "namedetails" Int :> QueryParam "extratags" Int :> QueryParam "statecode" Int :> QueryParam "showdistance" Int :> QueryParam "postaladdress" Int :> Verb 'GET 200 '[JSON] Location -- 'reverse' route
    :<|> "search.php" :> QueryParam "q" Text :> QueryParam "format" Text :> QueryParam "normalizecity" Int :> QueryParam "addressdetails" Int :> QueryParam "viewbox" Text :> QueryParam "bounded" Int :> QueryParam "limit" Int :> QueryParam "accept-language" Text :> QueryParam "countrycodes" Text :> QueryParam "namedetails" Int :> QueryParam "dedupe" Int :> QueryParam "extratags" Int :> QueryParam "statecode" Int :> QueryParam "matchquality" Int :> QueryParam "postaladdress" Int :> Verb 'GET 200 '[JSON] [Location] -- 'search' route
    :<|> Raw 


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype LocationIQClientError = LocationIQClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for LocationIQ.
-- The backend can be used both for the client and the server. The client generated from the LocationIQ OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createLocationIQClient@). Alternatively, provided
-- a backend, the API can be served using @runLocationIQMiddlewareServer@.
data LocationIQBackend m = LocationIQBackend
  { autocomplete :: Maybe Text -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> m [Value]{- ^ The Autocomplete API is a variant of the Search API that returns place predictions in response to an HTTP request.  The request specifies a textual search string and optional geographic bounds.  The service can be used to provide autocomplete functionality for text-based geographic searches, by returning places such as businesses, addresses and points of interest as a user types. The Autocomplete API can match on full words as well as substrings. Applications can therefore send queries as the user types, to provide on-the-fly place predictions. -}
  , balance :: m Balance{- ^ The Balance API provides a count of request credits left in the user's account for the day. Balance is reset at midnight UTC everyday (00:00 UTC). -}
  , directions :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m DirectionsDirections{- ^ Finds the fastest route between coordinates in the supplied order. -}
  , matching :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m DirectionsMatching{- ^ Matching API matches or snaps given GPS points to the road network in the most plausible way.  Please note the request might result multiple sub-traces.  Large jumps in the timestamps (> 60s) or improbable transitions lead to trace splits if a complete matching could not be found. The algorithm might not be able to match all points. Outliers are removed if they can not be matched successfully. -}
  , matrix :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Double -> Maybe Text -> m DirectionsMatrix{- ^ Computes duration of the fastest route between all pairs of supplied coordinates. Returns the durations or distances or both between the coordinate pairs. Note that the distances are not the shortest distance between two coordinates, but rather the distances of the fastest routes. -}
  , nearest :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> m DirectionsNearest{- ^ Snaps a coordinate to the street network and returns the nearest n matches. Where coordinates only supports a single {longitude},{latitude} entry. -}
  , reverse :: Maybe Double -> Maybe Double -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> m Location{- ^ Reverse geocoding is the process of converting a coordinate or location (latitude, longitude) to a readable address or place name. This permits the identification of nearby street addresses, places, and/or area subdivisions such as a neighborhood, county, state, or country. -}
  , search :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> m [Location]{- ^ The Search API allows converting addresses, such as a street address, into geographic coordinates (latitude and longitude). These coordinates can serve various use-cases, from placing markers on a map to helping algorithms determine nearby bus stops. This process is also known as Forward Geocoding. -}
  }

newtype LocationIQClient a = LocationIQClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative LocationIQClient where
  pure x = LocationIQClient (\_ -> pure x)
  (LocationIQClient f) <*> (LocationIQClient x) =
    LocationIQClient (\env -> f env <*> x env)

instance Monad LocationIQClient where
  (LocationIQClient a) >>= f =
    LocationIQClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO LocationIQClient where
  liftIO io = LocationIQClient (\_ -> liftIO io)

createLocationIQClient :: LocationIQBackend LocationIQClient
createLocationIQClient = LocationIQBackend{..}
  where
    ((coerce -> autocomplete) :<|>
     (coerce -> balance) :<|>
     (coerce -> directions) :<|>
     (coerce -> matching) :<|>
     (coerce -> matrix) :<|>
     (coerce -> nearest) :<|>
     (coerce -> reverse) :<|>
     (coerce -> search) :<|>
     _) = client (Proxy :: Proxy LocationIQAPI)

-- | Run requests in the LocationIQClient monad.
runLocationIQClient :: Config -> LocationIQClient a -> ExceptT ClientError IO a
runLocationIQClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runLocationIQClientWithManager manager clientConfig cl

-- | Run requests in the LocationIQClient monad using a custom manager.
runLocationIQClientWithManager :: Manager -> Config -> LocationIQClient a -> ExceptT ClientError IO a
runLocationIQClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a LocationIQClientError
callLocationIQ
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> LocationIQClient a -> m a
callLocationIQ env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (LocationIQClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the LocationIQ server at the provided host and port.
runLocationIQServer
  :: (MonadIO m, MonadThrow m)
  => Config -> LocationIQBackend (ExceptT ServerError IO) -> m ()
runLocationIQServer config backend = runLocationIQMiddlewareServer config requestMiddlewareId backend

-- | Run the LocationIQ server at the provided host and port.
runLocationIQMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> LocationIQBackend (ExceptT ServerError IO) -> m ()
runLocationIQMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serve (Proxy :: Proxy LocationIQAPI) (serverFromBackend backend)
  where
    serverFromBackend LocationIQBackend{..} =
      (coerce autocomplete :<|>
       coerce balance :<|>
       coerce directions :<|>
       coerce matching :<|>
       coerce matrix :<|>
       coerce nearest :<|>
       coerce reverse :<|>
       coerce search :<|>
       serveDirectoryFileServer "static")
