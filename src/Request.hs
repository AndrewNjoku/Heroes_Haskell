module Request
    ( getJSON
    ) where

import JSON

import GHC.Int
import qualified Data.ByteString.Lazy.Internal as L
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import qualified Data.ByteString as D

	

getJSON :: URL -> IO D.ByteString
getJSON x = simpleHttp x








