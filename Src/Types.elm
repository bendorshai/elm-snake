module Src.Types exposing (..)

import Matrix exposing (..)
import Time exposing (..)

-- Types

type Element = VoidElement | SnakeElement SnakeDisplay | AppleElement | ShroomElement Effect

type GameStatus = Play | GameOver Int

type Msg = 
      KeyDown KeyCode
    | Tick Time
    | Frame Time
    | NewFood Food

type SnakeDisplay = Regular | Sick | Invisible

type Food = Apple Location | Shroom Location Effect

-- Aliases

-- note: should Morph function accept effect or viseversa?

type alias Effect =     
    { startTime : Time
    -- Altering the user's sence of time 
    , tickDefinition : Time 
    -- Each cells morphing data
    , morphMatrix : Matrix MorphFunction
    -- The time the effect will last 
    , lifespanMillitics : Time
    }

type alias Shape = 
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  , rx : Float
  , ry : Float
  }

-- Given a shape and it's location, and it will morph
type alias MorphFunction = Game -> Location -> Shape -> Shape

type alias Score = Int

type alias Snake = 
    { body: List Location
    , direction : Location
    -- A location of a cell that may exist and may not, depending on whether snake is eating right now!
    -- basicly it stores the location of the previus last cell
    , trail : Location
    , display : SnakeDisplay
    }

type alias DisplayConfiguration =
    -- offset from left
    { offsetX : Int
    -- offset from top
    , offsetY : Int
    -- margin between two cells
    , margin : Int
    -- size of each cell in matrix
    , rectSize : Int
    -- radius of each matrix cell
    , radius : Int
    }

type alias GameConfiguration =
  -- Matrix length (width and height)
  { matrixLength : Int
  -- can bounce into wall?
  , cyclicmode : Bool
  }

type alias Game = 
  { matrix : Matrix Element
  , snake : Snake
  , food : Maybe Food
  , status : GameStatus
  , topscore : Score
  }

type alias Model = 
    { game : Game
    , effect : Maybe Effect  
    , displayConfiguration : DisplayConfiguration
    , gameConfiguration : GameConfiguration
    , time : Time
    }

type alias WaveFunction = Float -> Float

type alias KeyCode = Int

type alias ModelPredicate = (Model -> Bool)

type alias Direction = Matrix.Location