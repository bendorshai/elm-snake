
-- Internal imports
import Src.State
import Src.View
import Src.Init 

-- External imports
import Html exposing (program)

main =
  Html.program
    { init = Init.init
    , view = View.view
    , update = State.update
    , subscriptions = State.subscriptions
    }

