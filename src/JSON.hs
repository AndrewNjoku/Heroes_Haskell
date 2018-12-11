module Request
    ( getHeroes
	, getHeroesWin
    ) where


import Data.Aeson




	
data Hero =
  Hero { id  :: Int
       , name  :: !Text
       ,localised_name :: Int
       ,primary_attr :: !Text
       ,attack_type :: !Text
        
           } deriving (Show,Generic)

data HeroStats =
  Hero { id  :: Int
       , name  :: !Text
       ,localised_name :: Int
       ,primary_attr :: !Text
       ,attack_type :: !Text
        
           } deriving (Show,Generic)




instance FromJSON Hero
instance ToJSON Hero
