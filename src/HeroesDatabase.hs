module HeroesDatabase
	( database
    , closeDB
    , initialiseDB
    , insertHeroes
    , insertHeroesMatches
    , getHeroID
    ) where

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


{- | Create a DB connection, returns a DB handler.

-}
database :: IO Database
database = open $ pack "heroes.db"


closeDB = close

{- | Initialises the database creating tables if they don't exist already.

-}

initialiseDB :: Database -> IO ()
initialiseDB db = do
   exec db $ pack "CREATE TABLE IF NOT EXISTS heroes (\
            \id INTEGER PRIMARY KEY, \
            \name VARCHAR(100), \
			\localized_name VARCHAR(100), \
			\primary_attr VARCHAR(20), \
			\attack_type VARCHAR(20), \
			\legs INTEGER)"
   exec db $ pack "CREATE TABLE IF NOT EXISTS heroes_matches (\
            \hero_id INTEGER NOT NULL, \
            \duration INTEGER, \
            \league_name VARCHAR(150) DEFAULT NULL, \
            \kills INTEGER, \
            \deaths INTEGER, \
            \assists INTEGER, \
            \FOREIGN KEY (hero_id) REFERENCES heroes(id))"

data Hero = Hero {
	id :: Int64,
	name :: String,
	localized_name :: String,
	primary_attr :: String,
	attack_type :: String,
	legs :: Int64
} deriving (Eq, Show)

data HeroMatches = HeroMatches {
	hero_id :: Int64,
	duration :: Int64,
	league_name :: String,
	kills :: Int64,
	deaths :: Int64,
	assists :: Int64
} deriving (Eq, Show)

{- | Insert 'Hero' data type into heroes table.

-}

insertHeroes :: Database -> Hero -> IO ()
insertHeroes db hero = do
   let f (Hero i n l p a lg) = [ (pack ":i", SQLInteger i)
                                   , (pack ":n", SQLText (pack n))
                                   , (pack ":l", SQLFloat l)
                                   , (pack ":p", SQLFloat p)
                                   , (pack ":a", SQLFloat a)
                                   , (pack ":lg", SQLFloat lg)
                                   ]
   let args = f hero
   stmt <- prepare db (pack "INSERT INTO heroes VALUES (:i,:n,:l,:p,:a,:lg)")
   bindNamed stmt (f hero)
   result <- step stmt
   print result


{- | Insert 'HeroMatch' data type into heroes_matches table

-}
insertHeroesMatches :: Database -> Int64 -> HeroMatches -> IO ()
insertHeroesMatches db i match = do
   let f (HeroMatches i d l k d a) = [ (pack ":i", SQLInteger i)
                                   , (pack ":d", SQLText (pack d))
                                   , (pack ":l", SQLFloat l)
                                   , (pack ":k", SQLFloat k)
                                   , (pack ":dh", SQLFloat dh)
                                   , (pack ":a", SQLFloat a)
                                   ]
   let args = f match
   stmt <- prepare db (pack "INSERT INTO heroes_matches VALUES (:i,:d,:l,:k,:dh,:a)")
   bindNamed stmt (f match)
   result <- step stmt
   print result


getHeroID :: Database -> String -> IO Int64
getHeroID db name = do
   stmt <- prepare db (pack $ "SELECT (id) FROM heroes WHERE name=:cia")
   bindNamed stmt [ (pack ":cia", SQLText (pack name)) ]
   result <- step stmt       -- one statement step
   hero_db <- column stmt 0   -- read how returned
   let readID (SQLInteger n) = n
   let hero_id = readID hero_db
   return hero_id

{-
getHeroMatches :: Database -> String -> IO Double
getHeroMatches db hero_name = do
   hero_id <- getHeroID db hero_name
   -- getting list of prices
   stmt <- prepare db (pack "SELECT (close) FROM heroes_matches WHERE hero_id=:hid")
   bindNamed stmt [ (pack ":hid", SQLInteger hero_id) ]
   let
       isFloat (SQLFloat _) = True
       isFloat _ = False
   let getFloat (SQLFloat f) = f
   let readPrice ps = do
         result <- step stmt       -- one statement step
         p <- column stmt 0        -- read price
         if isFloat p then
            readPrice (p:ps)
         else
            return ps
   ps <- readPrice []
   let fs = map getFloat ps
   return $ (sum fs) / (read.show.length $ fs)

-}
