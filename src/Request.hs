module Request
    ( getHeroes
	, getHeroesWin
    ) where


import HeroesParser
import HeroesDatabase

import Database.SQLite3

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
	
type URL = String

getHeroes :: URL -> IO L.ByteString


getHeroesWin :: Int -> URL -> IO L.ByteString


