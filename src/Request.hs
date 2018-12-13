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








