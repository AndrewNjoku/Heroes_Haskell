module HeroesDatabase
	( database
    , closeDB
    , initialiseDB
    , insertHeroes
    , insertHeroesMatches
	, checkDataAlreadyLoaded
    , getHeroID
	, getAllData
	, getHeroDetailsByName
	, getHeroMatchesByName
	, getOverallStatsByName
	, getStatsByLeague
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

   
checkDataAlreadyLoaded :: Database -> Bool
checkDataAlreadyLoaded db = do
   stmt <- prepare db (pack $ "select (count(*)) from heroes")
   result <- step stmt       -- one statement step
   cia_db <- column stmt 0   -- read how returned
   let readID (SQLInteger n) = n
   let present = readID cia_db
   if present > 0 then
		return True
	else
		return False

getHeroDetailsByName :: Database -> String -> IO ()
getHeroDetailsByName db name = do
   stmt <- prepare db (pack "select id, name, localized_name, primary_attr, attack_type, legs, (select count(*) from heroes_matches where hero_id = id) matches from heroes where name like '%" ++ name ++ "%' or localized_name like '%" ++ name ++ "%' order by localized_name")
   
   putStrLn $ "Name" ++ " \t\t Localized name " ++ " \t\t Primary attribute " ++ " \t\t Attack type " ++ " \t\t Legs " ++ " \t\t Matches" ++ " "
   
   let
		isInt (SQLInteger _) = True
		isInt _ = False
	let getData = do
		result <- step stmt
		
		n <- column stmt 0
		nm <- columnText stmt 1
		l <- columnText stmt 2
		p <- columnText stmt 3
		a <- columnText stmt 4
		lg <- columnInt64 stmt 5
		m <- columnInt64 stmt 6
		
		if isInt n 
			then do 
				putStrLn $ "" ++ show(nm) ++ " \t " ++ show(l) ++ " \t " ++ show(p) ++ " \t " ++ show(a) ++ " \t " ++ show(lg) ++ " \t " ++ show(m) ++ " "
				getData
		else
			putStrLn ""
	getData

getHeroMatchesByName :: Database -> String -> IO ()
getHeroMatchesByName db name = do
   stmt <- prepare db (pack "select h.id, h.localized_name, hm.league_name, hm.kills, hm.deaths, hm.assists from heroes as h left join heroes_matches hm on h.id = hm.hero_id where name like '%" ++ name ++ "%' or localized_name like '%" ++ name ++ "%' order by localized_name")
   
   putStrLn $ "Localized name" ++ " \t\t League name " ++ " \t\t Kills " ++ " \t\t Deaths " ++ " \t\t Assists " ++ " "
   
   let
		isInt (SQLInteger _) = True
		isInt _ = False
	let getData = do
		result <- step stmt
		
		n <- column stmt 0
		nm <- columnText stmt 1
		l <- columnText stmt 2
		k <- columnInt64 stmt 3
		d <- columnInt64 stmt 4
		a <- columnInt64 stmt 5
		
		if isInt n 
			then do 
				putStrLn $ "" ++ show(nm) ++ " \t " ++ show(l) ++ " \t " ++ show(k) ++ " \t " ++ show(d) ++ " \t " ++ show(a) ++ " "
				getData
		else
			putStrLn ""
	getData
	
getOverallStatsByName :: Database -> String -> IO ()
getOverallStatsByName db name = do
    stmt <- prepare db (pack "select h.localized_name, sum(hm.kills), sum(hm.deaths), sum(hm.assists), count(*) matches from heroes as h left join heroes_matches hm on h.id = hm.hero_id where name like '%" ++ name ++ "%' or localized_name like '%" ++ name ++ "%' group by h.localized_name order by localized_name")
   
    putStrLn $ "Localized name" ++ " \t\t Total kills " ++ " \t\t Total deaths " ++ " \t\t Total assists " ++ " \t\t Total matches " ++ " "
   
    let
		isText (SQLText _) = True
		isText _ = False
	let getData = do
		result <- step stmt
		
		n <- column stmt 0
		l <- columnText stmt 0
		k <- columnInt64 stmt 1
		d <- columnInt64 stmt 2
		a <- columnInt64 stmt 3
		m <- columnInt64 stmt 4
		
		if isText n 
			then do 
				putStrLn $ "" ++ show(l) ++ " \t " ++ show(k) ++ " \t " ++ show(d) ++ " \t " ++ show(a) ++ " \t " ++ show(m) ++ " "
				getData
		else
			putStrLn ""
	getData

getStatsByLeague :: Database -> String -> IO ()
getStatsByLeague db name = do
    stmt <- prepare db (pack "select hm.league_name, count(distinct h.id) heroes, count(*) matches from heroes as h left join heroes_matches hm on h.id = hm.hero_id where league_name like '%" ++ name ++ "%' or league_name like '%" ++ name ++ "%' group by hm.league_name order by league_name")
   
    putStrLn $ "League name" ++ " \t\t Total heroes " ++ " \t\t Total matches " ++ " "
   
    let
		isText (SQLText _) = True
		isText _ = False
	let getData = do
		result <- step stmt
		
		n <- column stmt 0
		l <- columnText stmt 0
		h <- columnInt64 stmt 1
		m <- columnInt64 stmt 2
		
		if isText n 
			then do 
				putStrLn $ "" ++ show(l) ++ " \t " ++ show(h) ++ " \t " ++ show(m) " "
				getData
		else
			putStrLn ""
	getData











