{-# LANGUAGE BlockArguments #-}
module Main (main) where


import Fetch
import Parse

main :: IO ()
main = do
    let lat = "52.629729"
        lon = "-1.131592"
        date = "2023-01"  
        poly = Nothing  
        filePath = "D:\\Haskell\\Result\\crimes.json"  
    result <- fetchCrimeData (Just lat) (Just lon) poly (Just date)
    case result of 
        Left errMsg -> putStrLn $ "Error: " ++ errMsg
        Right byteString -> do
            let parseResult = parseCrimes byteString
            case parseResult of
                Left paraErr -> putStrLn $ "Error: " ++ paraErr
                Right crimes -> do
                    writeToJSON crimes filePath 
                    putStrLn $ "Data saved to " ++ filePath
-- comment