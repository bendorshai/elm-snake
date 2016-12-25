module Types exposing (..)

import Matrix exposing (..)
import Time exposing (..)

-- Types

type Element = VoidElement | SnakeElement SnakeDisplay | AppleElement

type GameStatus = Play | GameOver Int

type Msg = 
      KeyDown KeyCode
    | KeyUp KeyCode
    | Tick Time
    | NewApple Location

type SnakeDisplay = Regular | Sick | None

-- Aliases

type alias Score = Int

type alias Snake = 
    { body: List Location
    , direction : Location
    -- A location of a cell that may exist and may not, depending on whether snake is eating right now!
    -- basicly it stores the location of the previus last cell
    , trail : Location
    , display : SnakeDisplay
    }

type alias Apple = 
    { location: Location 
    }  

type alias Definition =
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
    -- Matrix length (width and height)
    , len : Int
    }

type alias Model = 
    { definition : Definition
    , matrix : Matrix.Matrix Element
    , snake : Snake
    , apple : Maybe Apple
    , status : GameStatus
    -- Wierd variables that affect the psychodalic effects
    , winds : Winds
    , topscore : Score
    }

type alias Winds = 
    { timeMultiplier : Float
    -- Will be used as a unit to add to cosinus function that applied on each rect size
    , noiseMatrix : Matrix Float
    }

type alias KeyCode = Int

type alias ModelPredicate = (Model -> Bool)

type alias Direction = Matrix.Location