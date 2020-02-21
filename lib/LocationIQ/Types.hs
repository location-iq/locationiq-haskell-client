{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module LocationIQ.Types (
  Address (..),
  Balance (..),
  Daybalance (..),
  DirectionsDirections (..),
  DirectionsDirectionsRoutes (..),
  DirectionsMatching (..),
  DirectionsMatrix (..),
  DirectionsMatrixSources (..),
  DirectionsNearest (..),
  DirectionsNearestWaypoints (..),
  Error (..),
  Location (..),
  Matchquality (..),
  Namedetails (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data Address = Address
  { addressHouseUnderscorenumber :: Maybe Text -- ^ 
  , addressRoad :: Maybe Text -- ^ 
  , addressResidential :: Maybe Text -- ^ 
  , addressBorough :: Maybe Text -- ^ 
  , addressNeighbourhood :: Maybe Text -- ^ 
  , addressQuarter :: Maybe Text -- ^ 
  , addressHamlet :: Maybe Text -- ^ 
  , addressSuburb :: Maybe Text -- ^ 
  , addressIsland :: Maybe Text -- ^ 
  , addressVillage :: Maybe Text -- ^ 
  , addressTown :: Maybe Text -- ^ 
  , addressCity :: Maybe Text -- ^ 
  , addressCityUnderscoredistrict :: Maybe Text -- ^ 
  , addressCounty :: Maybe Text -- ^ 
  , addressState :: Maybe Text -- ^ 
  , addressStateUnderscoredistrict :: Maybe Text -- ^ 
  , addressPostcode :: Maybe Text -- ^ 
  , addressCountry :: Maybe Text -- ^ 
  , addressCountryUnderscorecode :: Maybe Text -- ^ 
  , addressStateUnderscorecode :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Address where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "address")
instance ToJSON Address where
  toJSON = genericToJSON (removeFieldLabelPrefix False "address")


-- | 
data Balance = Balance
  { balanceStatus :: Maybe Text -- ^ 
  , balanceBalance :: Maybe Daybalance -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Balance where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "balance")
instance ToJSON Balance where
  toJSON = genericToJSON (removeFieldLabelPrefix False "balance")


-- | 
data Daybalance = Daybalance
  { daybalanceDay :: Maybe Int -- ^ 
  , daybalanceBonus :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Daybalance where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "daybalance")
instance ToJSON Daybalance where
  toJSON = genericToJSON (removeFieldLabelPrefix False "daybalance")


-- | 
data DirectionsDirections = DirectionsDirections
  { directionsDirectionsCode :: Maybe Text -- ^ 
  , directionsDirectionsWaypoints :: Maybe [Value] -- ^ 
  , directionsDirectionsRoutes :: Maybe [DirectionsDirectionsRoutes] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DirectionsDirections where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "directionsDirections")
instance ToJSON DirectionsDirections where
  toJSON = genericToJSON (removeFieldLabelPrefix False "directionsDirections")


-- | 
data DirectionsDirectionsRoutes = DirectionsDirectionsRoutes
  { directionsDirectionsRoutesLegs :: Maybe [Value] -- ^ 
  , directionsDirectionsRoutesWeightUnderscorename :: Maybe Text -- ^ 
  , directionsDirectionsRoutesGeometry :: Maybe Text -- ^ 
  , directionsDirectionsRoutesWeight :: Maybe Double -- ^ 
  , directionsDirectionsRoutesDistance :: Maybe Double -- ^ 
  , directionsDirectionsRoutesDuration :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DirectionsDirectionsRoutes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "directionsDirectionsRoutes")
instance ToJSON DirectionsDirectionsRoutes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "directionsDirectionsRoutes")


-- | 
data DirectionsMatching = DirectionsMatching
  { directionsMatchingCode :: Maybe Text -- ^ 
  , directionsMatchingTracepoints :: Maybe [Value] -- ^ 
  , directionsMatchingMatchings :: Maybe [Value] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DirectionsMatching where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "directionsMatching")
instance ToJSON DirectionsMatching where
  toJSON = genericToJSON (removeFieldLabelPrefix False "directionsMatching")


-- | 
data DirectionsMatrix = DirectionsMatrix
  { directionsMatrixCode :: Maybe Text -- ^ 
  , directionsMatrixDistances :: Maybe [Double] -- ^ 
  , directionsMatrixFallbackUnderscorespeedUnderscorecells :: Maybe [Double] -- ^ 
  , directionsMatrixSources :: Maybe [DirectionsMatrixSources] -- ^ 
  , directionsMatrixDestinations :: Maybe [DirectionsMatrixSources] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DirectionsMatrix where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "directionsMatrix")
instance ToJSON DirectionsMatrix where
  toJSON = genericToJSON (removeFieldLabelPrefix False "directionsMatrix")


-- | 
data DirectionsMatrixSources = DirectionsMatrixSources
  { directionsMatrixSourcesDistance :: Maybe Double -- ^ 
  , directionsMatrixSourcesLocation :: Maybe [Double] -- ^ 
  , directionsMatrixSourcesName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DirectionsMatrixSources where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "directionsMatrixSources")
instance ToJSON DirectionsMatrixSources where
  toJSON = genericToJSON (removeFieldLabelPrefix False "directionsMatrixSources")


-- | 
data DirectionsNearest = DirectionsNearest
  { directionsNearestCode :: Maybe Text -- ^ 
  , directionsNearestWaypoints :: Maybe [DirectionsNearestWaypoints] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DirectionsNearest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "directionsNearest")
instance ToJSON DirectionsNearest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "directionsNearest")


-- | 
data DirectionsNearestWaypoints = DirectionsNearestWaypoints
  { directionsNearestWaypointsNodes :: Maybe [Double] -- ^ 
  , directionsNearestWaypointsDistance :: Maybe Double -- ^ 
  , directionsNearestWaypointsLocation :: Maybe [Double] -- ^ 
  , directionsNearestWaypointsName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DirectionsNearestWaypoints where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "directionsNearestWaypoints")
instance ToJSON DirectionsNearestWaypoints where
  toJSON = genericToJSON (removeFieldLabelPrefix False "directionsNearestWaypoints")


-- | 
data Error = Error
  { errorError :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "error")
instance ToJSON Error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "error")


-- | 
data Location = Location
  { locationDistance :: Maybe Double -- ^ 
  , locationPlaceUnderscoreid :: Maybe Text -- ^ 
  , locationLicence :: Maybe Text -- ^ 
  , locationOsmUnderscoretype :: Maybe Text -- ^ 
  , locationOsmUnderscoreid :: Maybe Text -- ^ 
  , locationBoundingbox :: Maybe [Text] -- ^ 
  , locationLat :: Maybe Text -- ^ 
  , locationLon :: Maybe Text -- ^ 
  , locationDisplayUnderscorename :: Maybe Text -- ^ 
  , locationClass :: Maybe Text -- ^ 
  , locationType :: Maybe Text -- ^ 
  , locationImportance :: Maybe Double -- ^ 
  , locationAddress :: Maybe Address -- ^ 
  , locationNamedetails :: Maybe Namedetails -- ^ 
  , locationMatchquality :: Maybe Matchquality -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Location where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "location")
instance ToJSON Location where
  toJSON = genericToJSON (removeFieldLabelPrefix False "location")


-- | 
data Matchquality = Matchquality
  { matchqualityMatchcode :: Maybe Text -- ^ 
  , matchqualityMatchtype :: Maybe Text -- ^ 
  , matchqualityMatchlevel :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Matchquality where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "matchquality")
instance ToJSON Matchquality where
  toJSON = genericToJSON (removeFieldLabelPrefix False "matchquality")


-- | 
data Namedetails = Namedetails
  { namedetailsName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Namedetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "namedetails")
instance ToJSON Namedetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "namedetails")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do viceversa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
