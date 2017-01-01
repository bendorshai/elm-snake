module Src.State exposing (..)

-- Internal imports
import Src.Types exposing (..)
import Src.Init exposing (..)
import Src.Api exposing (..)

-- External imports
import Time exposing (..)
import Matrix exposing (..)
import Keyboard exposing (..)
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
                      -- Semantics
                      game = model.game
                      snake = model.game.snake

                      -- new direction, 
                      direction = keyCode |> keycodeToDirection snake.direction
                      throat = snakeThroat snake
                      
                      cyclicmode = model.gameConfiguration.cyclicmode
                      snakeInNewDirection = { snake | direction = direction}
                      newGame = 
                        { game 
                        | snake = snakeInNewDirection } 
                    in 
                        -- if not trying to go back to throat
                        if (stepSnakeHead newGame cyclicmode) /= throat
                        then
                            ( { model | game = newGame }
                            , Cmd.none 
                            )
                        else 
                            ( model, Cmd.none )
        Tick time -> step model time
        NewFood food ->
          let
            game = model.game
            newGame = 
             { game 
             | food = Just food }
          in
           ( { model
             | game = newGame }
           , Cmd.none
           )
        Frame time ->
          ( { model
            | time = time}
            , Cmd.none )

-- Steppers

step : Model -> Time -> (Model, Cmd Msg)
step model time =
    let
      game = model.game
      cyclicmode = model.gameConfiguration.cyclicmode
      newStatus = stepGameStatus game cyclicmode
    in
      case newStatus of 
        -- End of game over proccess
        GameOver 9 -> reInit model
        _ -> 
          let
            newSnake = stepSnake game cyclicmode newStatus 

            -- did snake just eat now?
            ate = List.length model.game.snake.body < List.length newSnake.body

            -- Remove the apple so snake will not eat it twice
            food = if ate then Nothing else model.game.food

            newMatrix = stepMatrix game
            newScore = stepScore game

            newConf = stepGameConfiguration model ate

            newGame = 
              { game
              | snake = newSnake
              , food = food
              , matrix = newMatrix
              , topscore = newScore
              , status = newStatus
              }
          in
            ( { model 
              | game = newGame
              , gameConfiguration = newConf
              }
            , stepCommand game ate
            )

stepGameConfiguration : Model -> Bool -> GameConfiguration
stepGameConfiguration model ate =   
  let
    currConf = model.gameConfiguration
    tickDef = currConf.tickDefinition
  in if tickDef > (80 * millisecond) && ate
  then
    { currConf
    | tickDefinition = tickDef - 10 * millisecond
    } 
  else
    currConf

{- TODO: what to do
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
    -}


stepDeadSnake : Game -> Snake
stepDeadSnake game =
    let 
        snakeDisplay = case game.status of
            -- should not happen 
            Play -> Regular
            GameOver n -> if n % 2 == 0 then Invisible else Sick
        
        snake = game.snake
    in
        { snake 
        | display = snakeDisplay }

stepScore : Game -> Score
stepScore game = 
  Basics.max (List.length game.snake.body) game.topscore

stepCommand : Game -> Bool -> Cmd Msg
stepCommand game ate = 
    if ate
    then
      commandRandomApple game.matrix
    else
      Cmd.none 
    
stepMatrix : Game -> Matrix Element
stepMatrix game = 
    Matrix.mapWithLocation (\loc elem -> locationToElement game loc) game.matrix 

stepLivingSnake : Game -> CyclicMode -> Snake
stepLivingSnake game cyclicmode =
    let 
        -- Sematix 
        snake = game.snake
        body = snake.body
        length = List.length body
        
        -- First and last cells
        currentHead = List.head body |> certainLocation
        exceptTail = List.take (length-1) body 
       
        -- new head and prev tail
        newHead = stepSnakeHead game cyclicmode
        prevTail = List.drop (length-1) body 
                        |> List.head 
                        |> certainLocation
        
        -- local new snake function
        newSnake : List Location -> Snake
        newSnake body = 
            Snake body game.snake.direction prevTail Regular
        
        -- next step potentials
        hungryNewBody = newHead :: exceptTail
        hungryNewSnake = newSnake hungryNewBody
        
        fullNewBody = List.append hungryNewBody [prevTail]
        fullNewSnake = newSnake fullNewBody

        eating = isEating { game 
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

stepSnakeHead : Game -> CyclicMode -> Location
stepSnakeHead game cyclicmode = 
  let
    snake = game.snake
    head = snakeHead snake
    
    naiveHead = addDirection head snake.direction
    x = Tuple.first naiveHead
    y = Tuple.second naiveHead

    isInBoard = 
      x >= 0 &&
      y >= 0 &&
      x < colCount game.matrix &&
      y < rowCount game.matrix
  in
    -- If the next naive step of snake will stay on board 
    -- or the game is not on cyclic mode, 
    -- return the naive step 
    if not cyclicmode ||
       isInBoard
    then 
      naiveHead
    -- else, it is cyclic mode and out of board step
    else
      let
        lengthx = colCount game.matrix
        lengthy = rowCount game.matrix 
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

-- TODO: combine stepSnake & stepGameStatus, to prevent moving snake twice to the future...

-- TODO: each step accepts game and gameConfiguration, it makes more sense...

stepSnake : Game -> CyclicMode -> GameStatus -> Snake
stepSnake game cyclicmode newStatus = 
  case newStatus of 
    GameOver _ -> stepDeadSnake game
    Play -> stepLivingSnake game cyclicmode

stepGameStatus : Game -> CyclicMode -> GameStatus
stepGameStatus game cyclicmode = 
    case game.status of
        -- Step in the proccess of gameover
        GameOver n -> GameOver (n+1)
        Play ->
            let
              -- Is snake gonna eat itself? or be outside of the borders in this turn?
              fictiveFuture = { game 
                              | snake = stepSnake game cyclicmode Play}
              
              predicatesToDie = 
                getDeathPredicates cyclicmode
              
              reasonsToDie = 
                List.map (\predicate -> predicate fictiveFuture) predicatesToDie
              
              shouldDie = List.any identity reasonsToDie
            in
              if shouldDie
              then 
                  GameOver 0
              else
                  Play

getDeathPredicates : CyclicMode -> List GamePredicate
getDeathPredicates cyclicmode = 
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
      frameSpan = second / fps
      effect = certainEffect model.gameConfiguration model.effect
    in 
      Sub.batch
          [ Keyboard.downs KeyDown
          -- Effects changes the tick time, so snake moves faster or slower
          , every effect.tickDefinition Tick
          -- framespan is constant
          , every frameSpan Frame 
          ]


-- Predicates

