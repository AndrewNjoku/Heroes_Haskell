module JSON
    ( getHeroes
	 , getHeroesWin
    ) where


import Data.Aeson
import GHC.Generics




	
data Hero =
  Hero { id  :: Int
       , name  :: !Text
       ,localised_name :: Int
       ,primary_attr :: !Text
       ,attack_type :: !Text
        
           } deriving (Show,Generic)

data HeroStats =
  Hero { name: !Text,
         localized_name:!Text,
         img: !Text,
         icon: !Text,
         pro_win: Int,
         pro_pick: 0,
         "hero_id": 0,
"pro_ban": 0,
"1_pick": 0,
"1_win": 0,
"2_pick": 0,
"2_win": 0,
"3_pick": 0,
"3_win": 0,
"4_pick": 0,
"4_win": 0,
"5_pick": 0,
"5_win": 0,
"6_pick": 0,
"6_win": 0,
"7_pick": 0,
"7_win": 0
} deiving (Show, Generic)



instance FromJSON Hero
instance ToJSON Hero
instance FromJSON HeroStats
instance  ToJSON HeroStats 

