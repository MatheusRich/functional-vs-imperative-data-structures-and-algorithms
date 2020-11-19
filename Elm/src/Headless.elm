port module Headless exposing (Flags, InputType, Model, Msg, OutputType, run)

import Html.Attributes exposing (list)
import LinkedList exposing (LinkedList(..))
import Platform exposing (Program)


type alias InputType =
    Int


type alias OutputType =
    String


port get : (InputType -> msg) -> Sub msg


port put : OutputType -> Cmd msg


run : (InputType -> OutputType) -> Program Flags Model Msg
run fn =
    Platform.worker
        { init = init
        , update = update fn
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = Input Int


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : (InputType -> OutputType) -> Msg -> Model -> ( Model, Cmd Msg )
update fn msg model =
    case msg of
        Input input ->
            ( model, put (fn input) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input
