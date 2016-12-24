module State exposing (..)

-- Internal imports
import Types exposing (..)

-- External imports
import Time exposing (..)
import Matrix exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import Array

-- Init functions

init : ( Model, Cmd Msg )
init =
    let
      definition = initDefinition

      l = definition.len
      -- position of snake
      p = l // 2
    in
  ( Model 
        definition 
        (initMatrix l l) 
        (initSnake p p) 
        (Just (initApple 1 1))
        Play
    , Cmd.none )

initMatrix : Int -> Int -> Matrix Element
initMatrix width height = 
    matrix width height (\location -> VoidElement)

initDefinition : Definition
initDefinition = 
    { offsetX = 25
    , offsetY = 25
    , margin = 10
    , rectSize = 25
    , radius = 10
    , len = 12
    }

initSnake : Int -> Int -> Snake 
initSnake x y = 
    { body = [loc x y]
    , direction = loc -1 0
    , trail = loc x y 
    }

initApple : Int -> Int -> Apple
initApple x y =
    Types.Apple (loc x y) 

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Update snake's direction
        KeyDown keyCode ->
            if model.status == GameOver
            then
                init
            else let 
                snake = model.snake
                direction = keyCode |> keycodeToDirection snake.direction
                head = snakeHead snake
                throat = snakeThroat snake
                snakeInNewDirection = { snake | direction = direction}
            in 
                -- if not trying to go back to throat
                if (addDirection head direction) /= throat
                then
                    ( { model | snake = snakeInNewDirection}
                    , Cmd.none 
                    )
                else 
                    ( model, Cmd.none )
        KeyUp keyCode ->
            ( model, Cmd.none )
        Tick time -> 
            let
              {- Step the whole world a bit, recive a new model of the world,
                 and a command that might affect the view or the model again... -}
              newModelNewCommand = step model
              newModel = Tuple.first newModelNewCommand
              newCommand = Tuple.second newModelNewCommand
            in
            ( newModel, newCommand )
        NewApple location -> 
            ( { model
              | apple = Just (Apple location) }
              , Cmd.none )

-- Steppers

step : Model -> (Model, Cmd Msg)
step model = 
    let
        newSnake = stepSnake model

        -- did snake just eat now?
        ate = List.length model.snake.body < List.length newSnake.body

        -- Remove the apple so snake will not eat it twice
        apple = if ate then Nothing else model.apple

        newMatrix = stepMatrix model
        newStatus = stepGameStatus model
    in
        ( { model 
          | snake = newSnake
          , matrix = newMatrix
          , status = newStatus
          , apple = apple }
        , stepCommand model ate
        )

stepCommand : Model -> Bool -> Cmd Msg
stepCommand model ate = 
    if ate
    then
        Cmd.batch 
          [ Random.generate NewApple (randomLocationGenerator model.matrix)
          , Cmd.none
          ]
    else
        Cmd.batch 
          [ Cmd.none 
          ]
    
stepMatrix : Model -> Matrix Element
stepMatrix model = 
    Matrix.mapWithLocation (\loc elem -> locationToElement model loc) model.matrix 

stepSnake : Model -> Snake
stepSnake model =
    let 
        -- Sematix 
        snake = model.snake
        body = snake.body
        length = List.length body
        
        -- First and last cells
        currentHead = List.head body |> certainLocation
        exceptTail = List.take (length-1) body 
       
        -- new head and prev tail
        newHead = addDirection currentHead model.snake.direction
        prevTail = List.drop (length-1) body 
                        |> List.head 
                        |> certainLocation
        
        newSnake : List Location -> Snake
        newSnake body = 
            Snake body model.snake.direction prevTail

        -- next step potentials
        hungryNewBody = newHead :: exceptTail
        hungryNewSnake = newSnake hungryNewBody
        
        fullNewBody = List.append hungryNewBody [prevTail]
        fullNewSnake = newSnake fullNewBody

        eating = isEating { model 
                          | snake = hungryNewSnake }
    in 
        if eating 
        then
            fullNewSnake
        else
            hungryNewSnake
        

stepGameStatus : Model -> GameStatus
stepGameStatus model = 
    let
      reasonsToDie = [
          model |> isInBorders |> not,
          model |> isEatingSelf
      ]

      shouldDie = List.any (\b -> b) reasonsToDie
    in
    if shouldDie
    then 
        GameOver
    else
        Play

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs Types.KeyDown
        , Keyboard.ups Types.KeyUp
        , Time.every (second/4) Types.Tick
        ]

-- Conversions

certainLocation : Maybe Location -> Location
certainLocation mayloc = 
    case mayloc of
        Just location -> location
        Nothing -> loc 0 0

keycodeToDirection :  Location -> Types.KeyCode -> Location
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

locationToElement : Model -> Location -> Element
locationToElement model location = 
    if List.member location model.snake.body
    then 
        SnakeElement
    else if maybeAppleInLocaion model.apple location
    then 
        AppleElement
    else 
        VoidElement

-- Predicates

-- Note: better get the whole model, in the future there might be some apples
isEating : ModelPredicate
isEating model = 
    let 
        apple = model.apple
        snake = model.snake
    in
        -- Poetic definition of is Eating:
        maybeAppleLocationIn apple snake.body   

isInBorders : ModelPredicate
isInBorders model = 
    let 
        mayloc = Matrix.get 
                    (snakeHead model.snake) 
                    model.matrix
    in case mayloc of 
        Just location -> True
        Nothing -> False 

isEatingSelf : ModelPredicate
isEatingSelf model = 
    let 
        body = model.snake.body

        countLocation = 
            countValueInList model.snake.body

        countMap = List.map countLocation body
        repeatedLocation = List.filter (\n -> n > 1) countMap
    in
        -- is there a location of snake that appears more then once?  
        List.length repeatedLocation > 0
        
-- Random Generators

randomLocationGenerator : Matrix a -> Generator Location
randomLocationGenerator matrix = 
    let 
        xGen = int 0 ((colCount matrix) - 1)
        yGen = int 0 ((rowCount matrix) - 1)
        randomPair = Random.pair xGen yGen
    in
        Random.map (\xy -> loc (Tuple.first xy) (Tuple.second xy)) randomPair

-- Snake api

snakeHead : Snake -> Location
snakeHead snake = 
    List.head snake.body |> certainLocation

-- second cell
snakeThroat : Snake -> Location
snakeThroat snake = 
    snake.body 
        |> Array.fromList
        |> Array.get 1
        |> certainLocation

-- Apple api
maybeAppleInLocaion : Maybe Apple -> Location -> Bool
maybeAppleInLocaion mayapple location = 
    case mayapple of
        Nothing -> False
        Just apple -> apple.location == location 

maybeAppleLocationIn : Maybe Apple -> List Location -> Bool
maybeAppleLocationIn mayapple list =
    case mayapple of 
        Nothing -> False
        Just apple -> List.member apple.location list

-- Utils

addDirection : Location -> Direction -> Location
addDirection location direction =
    loc (Tuple.first location + Tuple.first direction) 
        (Tuple.second location + Tuple.second direction)        

countValueInList : List a -> a -> Int
countValueInList list val = 
        list
        |> List.filter (\n -> n == val)
        |> List.length