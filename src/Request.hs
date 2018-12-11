module Request
    ( getHeroes
	, getHeroesWin
    ) where


import HeroesParser
import HeroesDatabase
import GHC.Int
import Data.Text (pack)
import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as B
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Conduit (runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Control.Exception as E
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString as D
import Network.HTTP.Req

	
type URL = String




getHeroes :: URL -> IO L.ByteString


  let payload = Hero
        [ "id" .= (10 :: Int)
        , "name" .= (20 :: String) ]


  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <- req GET -- method
       (https "https://api.opendota.com/api/heroes" /: "get") -- safe by construction URL
       (ReqBodyJson payload) -- use built-in options or add your own
       jsonResponse -- specify how to interpret response
       mempty       -- query 


getHeroesWin :: Int -> URL -> IO L.ByteString


