module JSON where


import Data.Aeson
import GHC.Generics




data Hero =

  Hero { id  :: Int
       , name  :: !Text
       ,localized_name :: !Text
       ,primary_attr :: !Text
       ,attack_type :: !Text
       ,roles     :: [Text]
       ,legs  :: Int
        
           } deriving (Show,Generic)


      
instance FromJSON Hero
instance ToJSON Hero



}           

data RecentMatches =
  RecentMaches {

      match_id :: Int64,
      start_time :: Int64,
      duration :: Int64,
      radiant_win :: String,
      leagueid :: Int64,
      league_name :: String,
      radiant :: String,
      player_slot :: Int64 ,
      account_id :: Int64,
      kills :: Int64,
      deaths :: Int64,
      assists :: Int64
      
} deriving (Show, Generic)


instance FromJSON RecentMaches
instance  ToJSON RecentMaches


