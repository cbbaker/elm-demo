module UsernameInput exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http

import Task
import Process
import Time

type Status = Ok | Checking | Invalid
type Timer = Off | Waiting

type alias Model =
    { uri : String
    , value : String
    , status : Status
    , timer : Timer
    }

type Msg = Input String
         | Lookup (Result Http.Error String)
         | TimesUp ()

init : String -> String -> ( Model, Cmd Msg )
init uri value =
    let model = Model uri value Ok Off
    in model ! [ check model ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            if model.timer == Waiting
            then { model | value = value } ! []
            else { model | value = value, timer = Waiting } ! [ setTimer ]

        TimesUp _ ->
            let newModel = { model | status = Checking, timer = Off }
            in newModel ! [ check newModel ]

        Lookup (Result.Ok _) ->
            { model | status = Invalid } ! []

        Lookup (Result.Err _) ->
            { model | status = Ok } ! []

        
setTimer : Cmd Msg
setTimer =
    500 * Time.millisecond |> Process.sleep |> Task.perform TimesUp

check : Model -> Cmd Msg
check model =
    model |> getRequest |> Http.send Lookup

getRequest : Model -> Http.Request String
getRequest { uri, value } =
    let url = uri ++ "/" ++ value
    in Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

view : Model -> Html Msg
view model =
    div [ classList [ ("form-group", True)
                    , ("has-error", model.status == Invalid)
                    ] 
        ]
    [ label [] [ text "User Name" ]
    , input [ type_ "text"
            , onInput Input
            , class "form-control"
            , value model.value
            ] []
    , span [ classList [ ("help-block", True)
                       , ("hide", model.status /= Invalid)
                       ]
           ]
          [ text "user name is taken" ]
    , span [ classList [ ("help-block", True)
                       , ("hide", model.status /= Checking)
                       ]
           ]
          [ text "checking user name" ]
    ]
    
