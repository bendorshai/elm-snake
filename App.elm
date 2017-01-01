
-- Internal imports
import Src.State
import Src.View
import Src.Init 

-- External imports
import Html exposing (program)

main =
  Html.program
    { init = Src.Init.init
    , view = Src.View.view
    , update = Src.State.update
    , subscriptions = Src.State.subscriptions
    }

