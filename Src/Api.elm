module Src.Api exposing (..)

import Src.Types exposing (..)
import Random exposing (..)
import Matrix exposing (..)

-- Food Api

commandRandomApple : Matrix Element -> Cmd Msg
commandRandomApple matrix = 
  Random.generate (\loc -> NewFood (Apple loc)) (randomLocationGenerator matrix) 

maybeFoodInLocaion : Maybe Food -> Location -> Bool
maybeFoodInLocaion mayfood location = 
  maybeFoodInLocaions mayfood [location]

maybeFoodInLocaions : Maybe Food -> List Location -> Bool
maybeFoodInLocaions mayfood list =
    case mayfood of 
        Nothing -> False
        Just (Apple location) -> List.member location list
        Just (Shroom location _) -> List.member location list




-- Random Generators

randomLocationGenerator : Matrix a -> Generator Location
randomLocationGenerator matrix = 
    let 
        xGen = int 0 ((colCount matrix) - 1)
        yGen = int 0 ((rowCount matrix) - 1)
        randomPair = Random.pair xGen yGen
    in
        Random.map (\xy -> loc (Tuple.first xy) (Tuple.second xy)) randomPair