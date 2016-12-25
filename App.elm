
-- Internal imports
import State
import View

-- External imports
import Html exposing (program)

main =
  Html.program
    { init = State.init Nothing
    , view = View.view
    , update = State.update
    , subscriptions = State.subscriptions
    }

