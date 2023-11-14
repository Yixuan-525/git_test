module Fetch 
(fetchCrimeData) where

--import Network.HTTP.Client.Conduit -- Provides HTTP request functionality
--import Network.HTTP.Simple  -- Extract the response body from the response of a network request
import Control.Exception (handle, SomeException) -- Exception handling functions and types
import qualified Data.ByteString.Lazy as L --For processing byte strings
import Network.HTTP.Conduit (simpleHttp)

-- |URL for requests to the Street Crime API
crimeApiUrl :: String
crimeApiUrl = "https://data.police.uk/api/crimes-street/all-crime"

-- |build querl URl
buildQueryUrl :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> String
buildQueryUrl (Just lat) (Just lon) _ date = crimeApiUrl ++ "?lat=" ++ lat ++ "&lng=" ++ lon ++ dateParam date
buildQueryUrl _ _ (Just poly) date = crimeApiUrl ++ "?poly" ++ poly ++ dateParam date
buildQueryUrl _ _ _ _ = crimeApiUrl

dateParam :: Maybe String -> String
dateParam (Just date) = "&date=" ++ date
dateParam Nothing = ""

-- | Make an HTTP GET request and securely handle the response
fetchCrimeData :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Either String L.ByteString)
fetchCrimeData lat lon poly date = handle (\e -> handleException $ show (e :: SomeException)) $ do
    let url = buildQueryUrl lat lon poly date
    response <- simpleHttp url
    return $ Right response

handleException :: String -> IO (Either String L.ByteString)
handleException information = return $ Left ("Failed to fetch data: " ++ information)