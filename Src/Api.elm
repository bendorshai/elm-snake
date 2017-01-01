module Src.Api exposing (..)
import Src.Types exposing (..)

import Random exposing (..)
import Matrix exposing (..)
import Keyboard exposing (..)
import Array exposing (..)
import Time exposing (..)

-- Food Api

commandRandomApple : Matrix Element -> Cmd Msg
commandRandomApple matrix = 
  Random.generate (\loc -> NewFood (Apple loc)) (randomLocationGenerator matrix) 

maybeFoodInLocation : Maybe Food -> Location -> Bool
maybeFoodInLocation mayfood location = 
  maybeFoodInLocations mayfood [location]

maybeFoodInLocations : Maybe Food -> List Location -> Bool
maybeFoodInLocations mayfood list =
    case mayfood of 
        Nothing -> False
        Just (food) -> List.member (foodLocation food) list

foodLocation : Food -> Location
foodLocation food =   
  case food of 
    Shroom location _ -> location
    Apple location -> location 

-- Random Generators

randomLocationGenerator : Matrix a -> Generator Location
randomLocationGenerator matrix = 
    let 
        xGen = int 0 ((colCount matrix) - 1)
        yGen = int 0 ((rowCount matrix) - 1)
        randomPair = Random.pair xGen yGen
    in
        Random.map (\xy -> loc (Tuple.first xy) (Tuple.second xy)) randomPair

-- Location Api


-- Conversions 

certainLocation : Maybe Location -> Location
certainLocation mayloc = 
    case mayloc of
        Just location -> location
        Nothing -> loc 0 0

keycodeToDirection :  Location -> KeyCode -> Location
keycodeToDirection defaultDirection key = 
    let 
        w = 87
        a = 65
        s = 83
        d = 68
    in
        if key == w then 
            loc 0 -1
        else if key == a then
            loc -1 0
        else if key == s then
            loc 0 1
        else if key == d then
            loc 1 0 
        else 
            defaultDirection

locationToElement : Game -> Location -> Element
locationToElement game location = 
    if List.member location game.snake.body
    then 
        SnakeElement game.snake.display
    else if maybeFoodInLocation game.food location
    then 
        AppleElement
    else 
        VoidElement

-- Snake api

-- Note: better get the whole model, in the future there might be some apples
isEating : GamePredicate
isEating game = 
    let 
        food = game.food
        snake = game.snake
    in
        -- Poetic definition of is Eating:
        maybeFoodInLocations food snake.body   

isInBorders : GamePredicate
isInBorders game = 
    let 
        mayloc = Matrix.get 
                    (snakeHead game.snake) 
                    game.matrix
    in case mayloc of 
        Just location -> True
        Nothing -> False 

isEatingSelf : GamePredicate
isEatingSelf game = 
    let 
        body = game.snake.body

        countLocation = 
            countValueInList game.snake.body

        countMap = List.map countLocation body
        repeatedLocation = List.filter (\n -> n > 1) countMap
    in
        -- is there a location of snake that appears more then once?  
        List.length repeatedLocation > 0

-- Snake api

snakeHead : Snake -> Location
snakeHead snake = 
    List.head snake.body |> certainLocation

-- second cell
snakeThroat : Snake -> Location
snakeThroat snake = 
    snake.body 
        |> Array.fromList
        |> Array.get 1 -- second cell
        |> certainLocation

-- other utils

addDirection : Location -> Direction -> Location
addDirection location direction =
    loc (Tuple.first location + Tuple.first direction) 
        (Tuple.second location + Tuple.second direction)        

countValueInList : List a -> a -> Int
countValueInList list val = 
        list
        |> List.filter (\n -> n == val)
        |> List.length

-- Effect api

noMorph : MorphFunction
noMorph game location shape = shape

certainEffect : GameConfiguration -> Maybe Effect -> Effect
certainEffect conf mayeff = 
  case mayeff of 
    Just effect -> effect
    Nothing -> 
      Effect 0 conf.tickDefinition noMorph 0 