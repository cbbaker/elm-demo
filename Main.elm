import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import UsernameInput

main : Program Never Model Msg
main = program
       { init = init
       , update = update
       , view = view
       , subscriptions = \_ -> Sub.none
       }

type alias Model =
    { usernameInputModel : UsernameInput.Model
    , password : String
    , passwordConfirmation : String
    , status : Status
    }

type Status = Ok
            | PasswordMismatch

init : ( Model, Cmd Msg )
init =
    let ( usernameInputModel, usernameInputCmd ) = UsernameInput.init "/usernames" "Joe"
    in validate { usernameInputModel = usernameInputModel
                , password = "s3kr3t"
                , passwordConfirmation = "asdf"
                , status = Ok
                } ! [ Cmd.map UsernameInputMsg usernameInputCmd ]

type Msg = UsernameInputMsg UsernameInput.Msg
         | Password String
         | PasswordConfirmation String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameInputMsg usernameInputMsg ->
            let ( usernameInputModel, usernameCmd ) = UsernameInput.update usernameInputMsg model.usernameInputModel
            in { model | usernameInputModel = usernameInputModel } ! [ Cmd.map UsernameInputMsg usernameCmd ]

        Password password ->
            validate { model | password = password } ! []

        PasswordConfirmation passwordConfirmation ->
            validate { model | passwordConfirmation = passwordConfirmation } ! []

validate : Model -> Model
validate model =
    let status = if model.password == model.passwordConfirmation
                 then Ok
                 else PasswordMismatch
    in { model | status = status }

view : Model -> Html Msg
view model =
    Html.form [] (viewFormInputs model)
        
viewFormInputs : Model -> List (Html Msg)
viewFormInputs {usernameInputModel, password, passwordConfirmation, status} =
    [ usernameInputModel |> UsernameInput.view |> Html.map UsernameInputMsg
    , viewPassword "Password" password status Password
    , viewPassword "Password Confirmation" passwordConfirmation status PasswordConfirmation
    ]
    
viewPassword : String -> String -> Status -> (String -> Msg) -> Html Msg
viewPassword labelText inputValue status handler  =
    div [ classList [ ("form-group", True)
                    , ("has-error", status /= Ok)
                    ] 
        ]
        [ label [] [ text labelText ]
        , input [ type_ "password"
                , onInput handler
                , class "form-control"
                , value inputValue
                ] []
        , span [ classList [ ("help-block", True)
                           , ("hide", status /= PasswordMismatch)
                           ]
               ]
            [ text "passwords don't match"]
        ]
