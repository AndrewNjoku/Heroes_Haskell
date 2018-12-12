module Main where

import Request.getJSON
import Data.Aeson




--Types

type URL = String
type HeroName = String



--URLS

baseurl :: String 
baseurl = "https://api.opendota.com/api/"

heroURL :: String
heroURL = baseurl++"/heroes"

heroRecentMatchesURL :: String -> String
heroStatsURL name = baseurl ++ getHeroId (name) ++ "/matches"



main :: IO ()

--get Hero data and convert to Hero datatype 

 d <- (eitherDecode <$> getJSON heroURL) :: IO (Either String [Hero])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
   case d of
        Left err -> putStrLn err
        Right ps -> print ps




getHeroId :: HeroName -> Int 

 getHeroId x | x=="Anti-Mage" = return 1
            | x="Axe"     = return 2
            | x="Bane" = return 3
            | x="Ax"     = return 4
            | x=="Anti-Mage" = return 5
            | x="Axe"     = return 6
            | x=="Anti-Mage" = return 1
            | x="Axe"     = return 27
            | x=="Anti-Mage" = return 1
            | x="Axe"     = return 2 
