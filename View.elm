module View exposing (..)

-- Internal imports
import Types exposing (..)

-- External imports
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Matrix exposing (..)

-- VIEW

view : Model -> Svg Msg
view model =
  let
    def = model.definition
    winds = model.winds
    matrix = model.matrix

    content = List.append (visualizeModel model) [
      -- TODO: should be visualized inside visualizeModel
      Svg.text_ [x "50", y "35", fontSize "35px"][Svg.text ("Score: " ++ toString model.topscore)]
    ]
  in
    svg [ viewBox "0 0 1500 1500", Svg.Attributes.width "1000px" ] content 
  

-- Visualizers

visualizeModel : Model -> List(Svg Msg)
visualizeModel model  =
    let 
      matrix = model.matrix

      superMatrix = 
          Matrix.mapWithLocation (visualizeElement model) matrix 
          |> Matrix.flatten
    in
      superMatrix
     

visualizeElement : Model -> Location -> Element -> Svg msg
visualizeElement model location element =
  let
    definition = model.definition
    winds = model.winds
    milliticks = model.milliticks

    xloc = Tuple.first location
    yloc = Tuple.second location

    -- Distance between drawing points of 2 recangles
    dist = definition.rectSize + definition.margin

    staticShape = 
      Shape
        {- x -} ((dist * xloc + definition.offsetX) |> toFloat)
        {- y -} ((dist * yloc + definition.offsetY) |> toFloat)
        {- width -} (definition.rectSize |> toFloat)
        {- height -} (definition.rectSize |> toFloat)
        {- rx -} (definition.radius |> toFloat)
        {- ry -} (definition.radius |> toFloat)
    
    shape = windShape model staticShape

    -- Apples are animated
    innerSvg = case element of 
      AppleElement -> appleAnimation
      SnakeElement _ -> snakeAnimation
      _ -> []
  in
    -- TODO: add style
    rect 
    [ shape.x |> toString |> x 
    , shape.y |> toString |> y 
    , shape.rx |> toString |> rx 
    , shape.ry |> toString |> ry 
    , shape.width  |> toString |> Svg.Attributes.width 
    , shape.height |> toString |> Svg.Attributes.height 
    , Html.Attributes.style (styleOf element)
    ] innerSvg

windShape : Model -> Shape -> Shape
windShape model shape = 
  let
    waveX = model.winds.nosiseFunctionX
    waveY = model.winds.nosiseFunctionY
    milliticks = model.milliticks

    frameRateDiv = 17

    shapeDiv = 
      toFloat (model.definition.margin + model.definition.rectSize)

    floaticks = toFloat (milliticks // frameRateDiv)
    amplitude = model.winds.amplitude
  in
    Shape 
    ( shape.x + amplitude * ( waveX ( (shape.x / shapeDiv) + floaticks ) ) )
    ( shape.y + amplitude * ( waveY ( (shape.y / shapeDiv) + floaticks ) ) )
    ( shape.width + ( amplitude / 2 ) * ( waveX ( shape.width + floaticks ) ) )
    ( shape.height + ( amplitude / 2 ) * ( waveY ( shape.height + floaticks ) ) )
    ( shape.rx + 0 * ( waveX ( shape.rx + floaticks ) ) )
    ( shape.ry + 0 * ( waveY ( shape.ry + floaticks ) ) )

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


