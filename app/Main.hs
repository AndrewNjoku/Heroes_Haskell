module Main where

import Request
import Data.Aeson
import JSON


main :: IO ()

main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getHeroes) :: IO (Either String [Hero])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of

 	-- Read error or return our hero object and parse it for further actions 

  Left err -> putStrLn err
  Right hero -> parseHero hero
