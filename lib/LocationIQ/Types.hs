{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module LocationIQ.Types (
  Address (..),
  Balance (..),
  Daybalance (..),
  Error (..),
  Location (..),
  Namedetails (..),
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data Address = Address
  { addressHouseUnderscorenumber :: Text -- ^ 
  , addressRoad :: Text -- ^ 
  , addressResidential :: Text -- ^ 
  , addressVillage :: Text -- ^ 
  , addressCounty :: Text -- ^ 
  , addressState :: Text -- ^ 
  , addressPostcode :: Text -- ^ 
  , addressCountry :: Text -- ^ 
  , addressCountryUnderscorecode :: Text -- ^ 
  , addressCity :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Address where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "address")
instance ToJSON Address where
  toJSON = genericToJSON (removeFieldLabelPrefix False "address")

-- | 
data Balance = Balance
  { balanceStatus :: Text -- ^ 
  , balanceBalance :: Daybalance -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Balance where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "balance")
instance ToJSON Balance where
  toJSON = genericToJSON (removeFieldLabelPrefix False "balance")

-- | 
data Daybalance = Daybalance
  { daybalanceDay :: Int -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Daybalance where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "daybalance")
instance ToJSON Daybalance where
  toJSON = genericToJSON (removeFieldLabelPrefix False "daybalance")

-- | 
data Error = Error
  { errorError :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "error")
instance ToJSON Error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "error")

-- | 
data Location = Location
  { locationPlaceUnderscoreid :: Text -- ^ 
  , locationLicence :: Text -- ^ 
  , locationOsmUnderscoretype :: Text -- ^ 
  , locationOsmUnderscoreid :: Text -- ^ 
  , locationBoundingbox :: [Text] -- ^ 
  , locationLat :: Text -- ^ 
  , locationLon :: Text -- ^ 
  , locationDisplayUnderscorename :: Text -- ^ 
  , locationClass :: Text -- ^ 
  , locationType :: Text -- ^ 
  , locationImportance :: Double -- ^ 
  , locationAddress :: Address -- ^ 
  , locationNamedetails :: Namedetails -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Location where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "location")
instance ToJSON Location where
  toJSON = genericToJSON (removeFieldLabelPrefix False "location")

-- | 
data Namedetails = Namedetails
  { namedetailsName :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Namedetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "namedetails")
instance ToJSON Namedetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "namedetails")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
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
