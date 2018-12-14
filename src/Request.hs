module Request
    ( getJSON
    ) where

import JSON


import Data.Aeson
import GHC.Int
import qualified Data.ByteString.Lazy.Internal as L
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import qualified Data.ByteString as D

	
--URLS

baseurl :: String 
baseurl = "https://api.opendota.com/api/"

heroURL :: String
heroURL = baseurl++"/heroes"

heroRecentMatchesURL :: String -> String
heroRecentMatchesURL name = baseurl ++ getHeroId (name) ++ "/matches"



Type HeroName = String

{- | Abstract function which will perform HTTP request on provided URL -}

getJSON :: URL -> IO D.ByteString
getJSON x = simpleHttp x


getHeroes :: IO ()

getHeroes = do

 putStrLn " Retrieving Hero info...."

 d <- (eitherDecode <$> getJSON heroURL) :: IO (Either String [Hero])

 {- | If d is returned to the Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 -}

   case d of
        Left err -> putStrLn err

        {- | In getHeroes right signifies our IO action has been successfull and we want to pass 
             our array of heroes to a Database function to further process. We do this using mapm_ as we 
             want to perform an action and are not worried about returning any data but performing the side 
             effect of adding these heroes to our database -}

        Right ps -> mapm_ insertHeroes ps



getRecentMatches :: Int -> IO [RecentMatches]



getRecentMatches x = do 

   d <- (eitherDecode <$> getJSON $ f) :: IO (Either String [RecentMatches])

                    where f = heroRecentMatchesURL x

     
     case d of
        Left err -> putStrLn err

        {- | In getHeroes right signifies our IO action has been successfull and we want to pass 
             our array of heroes to a Database function to further process. We do this using mapm_ as we 
             want to perform an action and are not worried about returning any data but performing the side 
             effect of adding these heroes to our database -}

        Right ps -> return ps 


	  





getHeroId :: HeroName -> String

 getHeroId x | x=="Anti-Mage" = "1"
             | x="Axe"     = return 2
             | x="Bane" = return 3
             | x="Ax"     = return 4
             | x=="Anti-Mage" = return 5
             | x="Axe"     = return 6
             | x=="Anti-Mage" = return 1
             | x="Axe"     = return 27
             | x=="Anti-Mage" = return 1
             | x="Axe"     = return 2 

