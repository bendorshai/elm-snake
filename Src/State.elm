module Src.State exposing (..)

-- Internal imports
import Src.Types exposing (..)
import Src.Init exposing (..)
import Src.Api exposing (..)

-- External imports
import Time exposing (..)
import Matrix exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import Array

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Update snake's direction
        KeyDown keyCode ->
            case model.game.status of 
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
        Tick time -> 
                let
                    {- Step the whole world a bit, recive a new model of the world,
                        and a command that might affect the view or the model again... -}
                    newModelNewCommand = step model
                    newModel = Tuple.first newModelNewCommand
                    newCommand = Tuple.second newModelNewCommand
                in
                    ( newModel, newCommand )
        NewFood location -> 
            ( { model
              | apple = Just (Food location) }
              , Cmd.none )
        Millitick time ->
          let
            newAmplitude = stepAmplitude model
            newMilliticks = stepMillitick model
            winds = model.winds
            newWinds = { winds
                       | amplitude = newAmplitude }

          in
          ( { model
            | milliticks = newMilliticks}
            , Cmd.none )

-- Steppers

step : Model -> (Model, Cmd Msg)
step model =
    let
      newStatus = stepGameStatus model
    in
      case newStatus of 
        -- End of game over proccess
        GameOver 9 -> reInit model
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
            newScore = stepScore model
          in
            ( { model 
              | snake = newSnake
              , matrix = newMatrix
              , status = newStatus
              , apple = apple 
              , topscore = newScore 
              }
            , stepCommand model ate
            )

stepAmplitude : Model -> Float
stepAmplitude model = 
    let
    -- 20 seonds (but i have an error in the calc)
    cycletime = ( 5 * 1000 ) |> toFloat

    -- 2 pi degrees in 1 cycletime
    degree = ( toFloat ( model.milliticks ) / cycletime ) * pi * 2
    
    zeroTwoRange = 1 + (cos (pi + degree))

    min = 0
    max = 1.3

    minMaxRange = ( ( zeroTwoRange / 2 ) * ( max-min ) ) + min
  in
    minMaxRange

-- amplitude changes over time from min value to max value
stepMillitick : Model -> Int
stepMillitick model =
  model.milliticks + 1

stepDeathSnake : Model -> Snake
stepDeathSnake model =
    let 
        snakeDisplay = case model.status of
            -- should not happen 
            Play -> Regular
            GameOver n -> if n % 2 == 0 then Invisible else Sick
        
        snake = model.snake
    in
        { snake 
        | display = snakeDisplay }

stepScore : Model -> Score
stepScore model = 
  Basics.max (List.length model.snake.body) model.topscore

stepCommand : Model -> Bool -> Cmd Msg
stepCommand model ate = 
    if ate
    then
        Cmd.batch 
          [ commandRandomApple model.game.matrix
          , Cmd.none
          ]
    else
        Cmd.batch 
          [ Cmd.none 
          ]
    
stepMatrix : Model -> Matrix Element
stepMatrix model = 
    Matrix.mapWithLocation (\loc elem -> locationToElement model loc) model.game.matrix 

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
        newHead = stepSnakeHead model
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

{-
stepWinds : Model -> Bool -> Winds
stepWinds model ate= 
    let 
        winds = model.winds

        -- If snake is eating reduce timeMultiplier, thus time will flow more quickly
        multiplier = if ate then 0.98 else 1 
    in
        { winds 
        | timeMultiplier = model.winds.timeMultiplier * multiplier }
        -}

stepSnakeHead : Model -> Location
stepSnakeHead model = 
  let
    mayhead = (Array.fromList model.snake.body) |> Array.get 0 
    
    head = case mayhead of 
      Just location -> location
      -- Exception
      Nothing -> loc 0 0 
    
    naiveHead = addDirection head model.snake.direction
    x = Tuple.first naiveHead
    y = Tuple.second naiveHead

    isInBoard = 
      x >= 0 &&
      y >= 0 &&
      x < colCount model.game.matrix &&
      y < rowCount model.game.matrix

  in
    -- If the next naive step of snake will stay on board 
    -- or the game is not on cyclic mode, 
    -- return the naive step 
    if not model.definition.cyclicmode ||
       isInBoard
    then 
      naiveHead
    -- else, it is cyclic mode and out of board step
    else
      let
        lengthx = colCount model.game.matrix
        lengthy = rowCount model.game.matrix 
      in
        stepCoordinateCyclic naiveHead lengthx lengthy

stepCoordinateCyclic : Location -> Int -> Int -> Location
stepCoordinateCyclic naiveHead lengthx lengthy =  
  let 
    x = Tuple.first naiveHead
    y = Tuple.second naiveHead

    newX = x % lengthx
    newY = y % lengthy
  in 
    loc newX newY

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
              
              predicatesToDie = 
                cyclicmodeToDeathPredicates model.definition.cyclicmode
              

              reasonsToDie = 
                List.map (\predicate -> fictiveFuture |> predicate) predicatesToDie

              shouldDie = List.any (\b -> b) reasonsToDie
            in
              if shouldDie
              then 
                  GameOver 0
              else
                  Play

cyclicmodeToDeathPredicates : Bool -> List (Model -> Bool)
cyclicmodeToDeathPredicates cyclicmode = 
  if cyclicmode
  then
    [ isEatingSelf ]
  else
    {- 
      Note: this is nice because it used to look like
      fictiveFuture |> isInBorders |> not,
      but since all I wanted is to leave the fictive future part 
      I couldn't now send the isInBorders Function into `not` function that accepts Bool
    -}
    [ isInBorders >> not
    , isEatingSelf 
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    let
      fps = 35
      frameSpan = millisecond / fps
    in 
      Sub.batch
          [ Keyboard.downs KeyDown
          , every (second * model.winds.timeMultiplier) Tick
          , every frameSpan Frame 
          ]

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

locationToElement : Model -> Location -> Element
locationToElement model location = 
    if List.member location model.snake.body
    then 
        SnakeElement model.snake.display
    else if maybeFoodInLocaion model.game.food location
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
                    model.game.matrix
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