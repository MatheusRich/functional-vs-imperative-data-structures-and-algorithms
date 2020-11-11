module Main exposing (..)

import LinkedList exposing (LinkedList(..))
import Platform exposing (worker)


nodeProgram : a -> Program () () ()
nodeProgram _ =
    worker
        { init = always ( (), Cmd.none )
        , update = \() -> \() -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }



-- worker
--     { init = always ( (), Cmd.none )
--     , update = update
--     , subscriptions = always (start Start)
--     }
-- main : Program () () Msg
-- main =


main : Program () () ()
main =
    nodeProgram (Debug.log "asdf")
