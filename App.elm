import Keyboard
import Matrix exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type Element = VoidElement | SnakeElement | AppleElement

type alias Location = Matrix.Location

type alias Snake = 
    { body: List Location
    , direction : Location
    }

type alias Apple = 
    { position: Location 
    }  

type alias Definition = 
    { offsetX : Int
    , offsetY : Int
    , margin : Int
    , rectSize : Int
    , radius : Int
    }

type alias Model = 
    { definition : Definition
    , matrix : Matrix.Matrix Element
    , snake : Snake
--    , apple : Apple
    }

type alias KeyCode = Int

init : ( Model, Cmd Msg )
init =
    let

      -- length of board
      l = 12

      -- position of snake
      p = l // 2
    in
  ( Model initDefinition (initMatrix l l) (initSnake p p) , Cmd.none )


initMatrix : Int -> Int -> Matrix.Matrix Element
initMatrix width height = 
    Matrix.matrix width height (\location -> VoidElement)

initDefinition : Definition
initDefinition = 
    { offsetX = 25
    , offsetY = 25
    , margin = 10
    , rectSize = 25
    , radius = 10
    }

initSnake : Int -> Int -> Snake 
initSnake x y = 
    { body = [Matrix.loc x y]
    , direction = Matrix.loc -1 0
    }

left = loc -1 0

-- UPDATE

type Msg = 
      KeyDown KeyCode
    | KeyUp KeyCode
    | Tick Time

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            ( model , Cmd.none )

        KeyUp keyCode ->
            ( model, Cmd.none )

        Tick time -> 
            ( apply model, Cmd.none)


apply : Model -> Model 
apply model = 
    { model | 
      matrix = Matrix.mapWithLocation 
        (\loc elem -> elementFromLocation model loc) 
        model.matrix 
    } 

elementFromLocation : Model -> Location -> Element
elementFromLocation model location = 
    if List.member location model.snake.body
    then SnakeElement
    else VoidElement 

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Time.every second Tick
        ]

-- VIEW

view : Model -> Html Msg
view model =
    let 
        entity = 
            Matrix.mapWithLocation (visualize model) model.matrix 
            |> Matrix.flatten
    in
     svg [ viewBox "0 0 1000 1000", Svg.Attributes.width "1000px" ] 
     entity

visualize : Model -> Matrix.Location -> Element -> Svg msg
visualize model location element =
  let
    width_ = toString (model.definition.rectSize)
    height_ = width_

    -- Distance between drawing points of 2 recangles
    dist = model.definition.rectSize + model.definition.margin // 2
    xloc = Tuple.first location
    yloc = Tuple.second location
    x_ = toString (dist * xloc + model.definition.offsetX) 
    y_ = toString (dist * yloc + model.definition.offsetY)
    r = toString (model.definition.radius)
  in
    -- TODO: add style
    rect 
    [ x x_
    , y y_
    , rx r
    , ry r
    , Svg.Attributes.width width_
    , Svg.Attributes.height height_
    , Html.Attributes.style (styleOf element)
    ] []


styleOf : Element -> List (String, String)
styleOf element = 
    case element of
        VoidElement -> voidStyle
        SnakeElement -> snakeStyle
        AppleElement -> appleStyle

voidStyle : List (String, String)
voidStyle = cellStyle "green"

snakeStyle : List (String, String)
snakeStyle = cellStyle "black"

appleStyle : List (String, String)
appleStyle = cellStyle "red"

cellStyle : String -> List (String, String)
cellStyle color = 
          [ ("fill"     , color)
          , ("stroke"      , "black")
          , ("strokeWidth"     , "2")
          , ("opacity"    , "0.85")
          ]          