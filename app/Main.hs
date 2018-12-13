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

check :: Bool
chec









main :: IO ()

--get Hero data and convert to Hero datatype 

main = do 
    


    putStrLn "Welcome to Dota 2 HeroDex"

    --Check if we have stored the heroes in database already
    --if we have not we will call the getHeroes method in Request module to make the 
    --Neccessery HTTP request 

    putStrLn "Loading Heroes ...."

    
    	
	a <- checkDataAlreadyLoaded 

	   case a of 
	   	    True makeRequest -> getHeroes 
	   	    False dataLoaded -> putStrLn "Data Already Loaded"







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
