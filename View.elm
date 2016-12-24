module View exposing (..)

-- Internal imports
import Types exposing (..)

-- External imports
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Matrix exposing (..)

-- VIEW

view : Model -> Html Msg
view model =
    if model.status == Play
    then
        visualizeMatrix model.definition model.matrix
    else 
        visualizeGameover model.definition

-- Visualizers

visualizeGameover : Definition -> Html Msg
visualizeGameover definition =
    let
      width_ = 5
    in
        Html.img [src "assets/gameover.png", Html.Attributes.style (imageStyle definition)] []

visualizeMatrix : Definition -> Matrix Element -> Html Msg
visualizeMatrix definition matrix =
    let 
        entity = 
            Matrix.mapWithLocation (visualizeElement definition) matrix 
            |> Matrix.flatten
    in
     svg [ viewBox "0 0 1000 1000", Svg.Attributes.width "1000px" ] 
     entity

visualizeElement : Definition -> Location -> Element -> Svg msg
visualizeElement definition location element =
  let
    width_ = toString (definition.rectSize)
    height_ = width_

    -- Distance between drawing points of 2 recangles
    dist = definition.rectSize + definition.margin // 2
    xloc = Tuple.first location
    yloc = Tuple.second location
    x_ = toString (dist * xloc + definition.offsetX) 
    y_ = toString (dist * yloc + definition.offsetY)
    r = toString (definition.radius)
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

-- Styles

styleOf : Element -> List (String, String)
styleOf element = 
    case element of
        Types.VoidElement -> voidStyle
        Types.SnakeElement -> snakeStyle
        Types.AppleElement -> appleStyle

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

imageStyle : Definition -> List (String, String)
imageStyle definition = 
    let
      top = toString definition.offsetY ++ "px"
      left = toString definition.offsetX ++ "px"
      length = 
        -- length of all rects
        definition.len * definition.rectSize + 
        -- length between rects
        definition.margin * (definition.len - 1)
        |> toString 
        |> (++) "px"
    in
    [ ("top" , top)
    , ("left" , left)
    , ("width" , length)
    , ("height" , length)
    , ("position" , "relative")
    ]

{-
type alias Definition = 
    { offsetX : Int
    , offsetY : Int
    , margin : Int
    , rectSize : Int
    , radius : Int
    }
    
-}