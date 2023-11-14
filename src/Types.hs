{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text (Text)
import Data.Aeson
import Control.Monad (mzero)
-- | Approximate location of the incident
data Location = Location
  {
    latitude :: Text,
    longitude :: Text,
    street :: Street
  } deriving (Eq, Show)

-- | The approximate street the crime occurred
data Street = Street
  {
    streetId :: Int,
    streetName :: Text
  } deriving (Eq, Show)

-- | The category and date of the latest recorded outcome for the crime
data OutcomeStatus = OutcomeStatus
  {
    category :: Maybe Text,
    date :: Maybe Text
  } deriving (Eq, Show)

-- | Crime information
data Crime = Crime
  {
    crimeCategory :: Text, --Category of the crime
    persistentId :: Maybe Text, --64-character unique identifier for that crime. (This is different to the existing 'id' attribute, which is not guaranteed to always stay the same for each crime.)
    month :: Text, --Month of the crime
    location :: Location, 
    context :: Maybe Text, --Extra information about the crime (if applicable)
    apiId :: Int, --ID of the crime. This ID only relates to the API, it is NOT a police identifier
    locationType :: Text, --The type of the location. Either Force or BTP: Force indicates a normal police force location; BTP indicates a British Transport Police location. BTP locations fall within normal police force boundaries.
    locationSubtype :: Maybe Text, --For BTP locations, the type of location at which this crime was recorded.
    outcomeStatus :: Maybe OutcomeStatus
  } deriving (Eq, Show)

  -- | Convert to json data type
instance ToJSON Location where
  toJSON (Location lat longitude street) =
    object [ "latitude" .= lat
           , "longitude" .= longitude
           , "street" .= street
           ]

instance ToJSON Street where
  toJSON (Street streetId streetName) =
    object [ "streetId" .= streetId
           , "streetName" .= streetName
           ]

instance ToJSON OutcomeStatus where
  toJSON (OutcomeStatus category date) =
    object [ "category" .= category
           , "date" .= date
           ]

instance ToJSON Crime where
  toJSON (Crime crimeCategory persistentId month location context apiId locationType locationSubtype outcomeStatus) =
    object [ "crimeCategory" .= crimeCategory
           , "persistentId" .= persistentId
           , "month" .= month
           , "location" .= location
           , "context" .= context
           , "apiId" .= apiId
           , "locationType" .= locationType
           , "locationSubtype" .= locationSubtype
           , "outcomeStatus" .= outcomeStatus
           ]

-- | Parse Location objects
instance FromJSON Location where
  parseJSON (Object v) =
    Location <$> v .: "latitude"
             <*> v .: "longitude"
             <*> v .: "street"
  parseJSON _ = mzero

-- | Parse Street objects
instance FromJSON Street where
  parseJSON (Object v) =
    Street <$> v .: "id"
           <*> v .:"name"
  parseJSON _ = mzero

-- | Parse the OutcomeStatus object
instance FromJSON OutcomeStatus where
  parseJSON (Object v) =
    OutcomeStatus <$> v .:? "category"
                  <*> v .:? "date"
  parseJSON _ = mzero

-- | Parse the Crime object
instance FromJSON Crime where
  parseJSON (Object v) =
    Crime <$> v .: "category"
          <*> v .:? "persistent_id"
          <*> v .: "month"
          <*> v .: "location"
          <*> v .:? "context"
          <*> v .: "id"
          <*> v .: "location_type"
          <*> v .:? "location_subtype"
          <*> v .:? "outcome_status"
  parseJSON _ = mzero
