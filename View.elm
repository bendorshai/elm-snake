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

view : Model -> Svg Msg
view model =
  let
    content = List.append (visualizeMatrix model.definition model.matrix) [
      Svg.text_ [x "50", y "35", fontSize "35px"][Svg.text ("Score: " ++ toString model.topscore)]
    ]
  in
    svg [ viewBox "0 0 1500 1500", Svg.Attributes.width "1000px" ] content 
  
    
        

-- Visualizers

visualizeMatrix : Definition -> Matrix Element -> List(Svg Msg)
visualizeMatrix definition matrix =
    let 
        superMatrix = 
            Matrix.mapWithLocation (visualizeElement definition) matrix 
            |> Matrix.flatten
    in
        superMatrix
     

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

    -- Apples are animated
    innerSvg = case element of 
      AppleElement -> appleAnimation
      SnakeElement _ -> snakeAnimation
      _ -> []
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
    ] innerSvg

-- Animations
snakeAnimation : List (Svg msg)
snakeAnimation = 
    [ animate 
      [ attributeType "svg"
      , attributeName "rx"
      , values "15;0;15"
      , dur "2s"
      , repeatCount "indefinite"
      ] []
    ]

appleAnimation : List (Svg msg)
appleAnimation = 
    [ animate 
      [ attributeType "svg"
      , attributeName "rx"
      , values "15;30;15"
      , dur "1.94s"
      , repeatCount "indefinite"
      ] []  
      ,
      animate 
      [ attributeType "svg"
      , attributeName "ry"
      , values "15;30;15"
      , dur "1.96s"
      , repeatCount "indefinite"
      ] []  
    ] 

-- Styles

styleOf : Element -> List (String, String)
styleOf element = 
    case element of
        Types.VoidElement -> voidStyle
        Types.SnakeElement display -> snakeStyle display
        Types.AppleElement -> appleStyle

voidStyle : List (String, String)
voidStyle = cellStyle "green"

snakeStyle : SnakeDisplay -> List (String, String)
snakeStyle display = 
    if display == None
    then
        voidStyle
    else if display == Sick
    then
        cellStyle "grey"
    else
        cellStyle "black"
    

appleStyle : List (String, String)
appleStyle = cellStyle "red"

cellStyle : String -> List (String, String)
cellStyle color = 
          [ ("fill"     , color)
          , ("stroke"      , "black")
          , ("strokeWidth"     , "2")
          , ("opacity"    , "0.85")
          ]


