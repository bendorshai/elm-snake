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
init  =
    let
      definition = initDefinition

      l = definition.len
      -- position of snake
      p = l // 2

      matrix = (initMatrix l l) 
    in
  ( Model 
        definition 
        matrix
        (initSnake p p) 
        Nothing -- apple
        Play
        (initWinds 0.25 l l)
        0
    , commandRandomApple matrix)

reInit : Maybe Apple -> Score -> ( Model, Cmd Msg )
reInit mayapp score =
    let
      initializedModelCommand = init
      tempModel = Tuple.first initializedModelCommand

      newMatrix = 
        case mayapp of
          Nothing -> tempModel.matrix
          -- place apple in matrix
          Just apple -> Matrix.set apple.location AppleElement tempModel.matrix
      
      model = { tempModel
              | matrix = newMatrix 
              , apple = mayapp
              , topscore = score }

      command = 
        case mayapp of 
          Nothing -> commandRandomApple newMatrix
          Just apple -> Cmd.none 
    in
  ( model, command )

initMatrix : Int -> Int -> Matrix Element
initMatrix width height = 
    matrix width height (\location -> VoidElement)

initDefinition : Definition
initDefinition = 
    { offsetX = 50
    , offsetY = 50
    , margin = 15
    , rectSize = 40
    , radius = 15
    , len = 12
    }

initSnake : Int -> Int -> Snake 
initSnake x y = 
    { body = [loc x y]
    , direction = loc -1 0
    , trail = loc x y 
    , display = Regular
    }

initApple : Int -> Int -> Apple
initApple x y =
    Types.Apple (loc x y) 

initWinds : Float -> Int -> Int -> Winds
initWinds timeMult width heigth = 
    Winds timeMult (matrix width heigth (\location -> 0.0)) 

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Update snake's direction
        KeyDown keyCode ->
            case model.status of 
                -- Keys don't work when game is over
                GameOver _ -> ( model, Cmd.none )
                Play ->
                    let 
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

stepDeathSnake : Model -> Snake
stepDeathSnake model =
    let 
        snakeDisplay = case model.status of
            -- should not happen 
            Play -> Regular
            GameOver n -> if n % 2 == 0 then None else Sick
        
        snake = model.snake
    in
        { snake 
        | display = snakeDisplay }

step : Model -> (Model, Cmd Msg)
step model =
    let
      newStatus = stepGameStatus model
    in
      case newStatus of 
        -- End of game over proccess
        GameOver 9 -> reInit model.apple model.topscore
        _ -> 
          let
            newSnake = 
              case newStatus of 
                Play -> stepSnake model
                GameOver n -> stepDeathSnake model

            -- did snake just eat now?
            ate = List.length model.snake.body < List.length newSnake.body

            -- Remove the apple so snake will not eat it twice
            apple = if ate then Nothing else model.apple

            newMatrix = stepMatrix model
            newWinds = stepWinds model ate
            newScore = stepScore model
          in
            ( { model 
              | snake = newSnake
              , matrix = newMatrix
              , status = newStatus
              , apple = apple 
              , winds = newWinds
              , topscore = newScore 
              }
            , stepCommand model ate
            )

stepScore : Model -> Score
stepScore model = 
  Basics.max (List.length model.snake.body) model.topscore

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
            Snake body model.snake.direction prevTail Regular
        
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

stepWinds : Model -> Bool -> Winds
stepWinds model ate= 
    let 
        winds = model.winds

        -- If snake is eating reduce timeMultiplier, thus time will flow more quickly
        multiplier = if ate then 0.98 else 1 
    in
        { winds 
        | timeMultiplier = model.winds.timeMultiplier * multiplier }

stepGameStatus : Model -> GameStatus
stepGameStatus model = 
    case model.status of
        -- Step in the proccess of gameover
        GameOver n -> GameOver (n+1)
        Play ->
            let
              -- Is snake gonna eat itself? or be outside of the borders in this turn?
              fictiveFuture = { model 
                              | snake = stepSnake model }

              reasonsToDie = [
                  fictiveFuture |> isInBorders |> not,
                  fictiveFuture |> isEatingSelf
              ]

              shouldDie = List.any (\b -> b) reasonsToDie
            in
              if shouldDie
              then 
                  GameOver 0
              else
                  Play


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =

    Sub.batch
        [ Keyboard.downs Types.KeyDown
        , Keyboard.ups Types.KeyUp
        ,  Time.every (second * model.winds.timeMultiplier) Types.Tick
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
        SnakeElement model.snake.display
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

commandRandomApple : Matrix Element -> Cmd Msg
commandRandomApple matrix = 
  Random.generate NewApple (randomLocationGenerator matrix) 

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