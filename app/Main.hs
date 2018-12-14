module Main where

import Request.getJSON
import Data.Aeson




--Types

type URL = String
type HeroName = String




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

   putStrLn "Please enter the name of the Hero you want to learn about with modifier: -h: basic hero info, -rm return recent matches played with this hero ,for hero names enter help()

   x <- getLine 

   z <- checkHeroName x
      
       case z of

         True -> getRecentMatches x 
         False putStrLn "Hero name entered incorrectly, for a list of Heroes please enter help() "




checkInput :: (HeroModifier l) => l -> 


