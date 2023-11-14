module Parse
  ( writeToJSON
  , parseCrimes
  ) where


import Types
import Data.Aeson
import Data.ByteString.Lazy as L

-- | Parse JSON file into Crime list
parseCrimes :: L.ByteString -> Either String [Crime]
parseCrimes = eitherDecode

-- | Convert to json file
writeToJSON :: [Crime] -> FilePath -> IO ()
writeToJSON crimes filePath = L.writeFile filePath (encode crimes)