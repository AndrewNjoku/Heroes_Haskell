module Main where

import System.IO
import Request
import HeoresDatabase
import Data.Aeson
import Data.Text
import System.Environment




--Types

type URL = String
type HeroName = Stri


rawHeroName::String ->String
rawHeroName x = drop 2 x



--modifierd
heroInfoModifier :: Text
heroInfoModifier = "-i".pack

recentMatchesModifier :: Text 
recentMatchesModifier = "-r".pack

averageMatchesModifier :: Text 
averageMatchesModifier = "-a".pack

leaguePerformanceModifier :: Text 
averageMatchesModifier = "-l".pack

--  Database instance 



--get Hero data and convert to Hero datatype 


main :: IO()

main = do 

--our db istance

	dbase <- database
    
    
    putStrLn "Welcome to Dota 2 HeroDex, Loading heroes...."

    --Check if we have stored the heroes in database already
    --if we have not we will call the getHeroes method in Request module to make the 
    --Neccessery HTTP request 

    

	a <- checkDataAlreadyLoaded 

	   case a of 
	   	    True makeRequest -> "HeroDex is up to date with all the latest hero information"

	   	    False dataLoaded -> initialiseDB dbase

    putStrLn "to find out more about any dota 2 hero please type their name plus modifier with no spaces: -i for general info , -r for recent match stats, -a for an averaged version, and -l for league based stats

-- reading in user input 

    x <- getLine 

-- converting to a Text haskell type so we can make comparisons 
    y <- pack x

--using the isinfix method to check if the user input contains specified modifier
    q <- isInfixOf heroInfoModifier y

       case q of


-- we use drop here to remove the moifier when making the call to collect the information
       	    True -> getHeroDetailsByName dbase (rawHeroName (X))
       	    False -> return

    t <-  isInfixOf recentMatchesModifier y

        case t of 

            True -> getHeroMatchesByName dbase (rawHeroName (X))
            False "Dunoo"

             


    m <-  isInfixOf averageMatchesModifier y

        case t of 

            True -> getOverallStatsByName dbase (rawHeroName (X))
            False "Dunoo"


    m <-  isInfixOf leaguePerformanceModifier y

        case t of 

            True -> getStatsByLeague dbase (rawHeroName (X))
            
            False "Dunoo"

             


          

             








  





   

