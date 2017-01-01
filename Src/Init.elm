module Src.Init exposing (..)

import Src.Types exposing (..)
import Src.Api exposing (..)
import Time exposing (..)
import Matrix exposing (..)

-- Init functions

init : ( Model, Cmd Msg )
init =
    let
      -- Init everithing
      diaplayConf = initDisplayConfiguration
      gameConf = initGameConfiguration
      game = initGame gameConf
      effect = Nothing
    in
  ( Model 
      game
      effect
      diaplayConf
      gameConf
      0
    , commandRandomApple game.matrix)

initGame : GameConfiguration -> Game
initGame config = 
  let 
    len = config.matrixLength 
    pos = len//2 
    food = Nothing
    score = 0
  in 
    Game
      (initMatrix len len)
      (initSnake pos pos) 
      food
      Play
      score

initMatrix : Int -> Int -> Matrix Element
initMatrix width height = 
    matrix width height (\location -> VoidElement)

initDisplayConfiguration : DisplayConfiguration
initDisplayConfiguration = 
    { offsetX = 50
    , offsetY = 50
    , margin = 7
    , rectSize = 40
    , radius = 15
    }

initGameConfiguration : GameConfiguration
initGameConfiguration = 
  { matrixLength = 12
  , cyclicmode = True
  , tickDefinition = second / 3
  }

initSnake : Int -> Int -> Snake 
initSnake x y = 
    { body = [loc x y]
    , direction = loc -1 0
    , trail = loc x y 
    , display = Regular
    }

reInit : Model -> ( Model, Cmd Msg )
reInit model =
    let
      mayfood = model.game.food
      score = model.game.topscore
      effect = model.effect 

      initializedModelCommand = init
      tempModel = Tuple.first initializedModelCommand

      newMatrix = 
        case mayfood of
          Nothing -> tempModel.game.matrix
          -- place apple in matrix
          Just (Apple location) -> 
            Matrix.set location AppleElement tempModel.game.matrix
          Just (Shroom location effect) -> 
            Matrix.set location (ShroomElement effect) tempModel.game.matrix

      initializedGame = initGame model.gameConfiguration

      newGame = 
        { initializedGame
        | matrix = newMatrix
        , food = mayfood
        , topscore = score
        }          

      newModel = 
        { tempModel
        | game = newGame 
        , effect = effect 
        }

      newCommand = 
        case mayfood of 
          Nothing -> commandRandomApple newMatrix
          Just apple -> Cmd.none 
    in
  ( newModel, newCommand )