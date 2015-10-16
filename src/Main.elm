import ObjectModel
import StartApp.Simple exposing (start)

main =
  start { model  = ObjectModel.init
        , update = ObjectModel.update
        , view   = ObjectModel.view
        }
