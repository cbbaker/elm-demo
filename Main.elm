import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http

main : Program Never Model Msg
main = program
       { init = init
       , update = update
       , view = view
       , subscriptions = \_ -> Sub.none
       }

type alias Model =
    { username : String
    , password : String
    , passwordConfirmation : String
    , status : Status
    }

type Status = Ok
            | Checking
            | Error ErrorType

type ErrorType = PasswordMismatch
               | UsernameTaken

init : ( Model, Cmd Msg )
init =
    validate { username = "Joe"
             , password = "s3kr3t"
             , passwordConfirmation = "asdf"
             , status = Ok
             } ! []

type Msg = Username String
         | Password String
         | PasswordConfirmation String
         | CheckUsernameResult (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username ->
            { model | username = username, status = Checking } ! [ checkUsername username ]

        Password password ->
            validate { model | password = password } ! []

        PasswordConfirmation passwordConfirmation ->
            validate { model | passwordConfirmation = passwordConfirmation } ! []

        CheckUsernameResult (Result.Ok _) ->
            { model | status = Error UsernameTaken } ! []

        CheckUsernameResult (Result.Err err) ->
            validate { model | status = Ok } ! []


validate : Model -> Model
validate model =
    case model.status of
        Checking ->
            model

        Error UsernameTaken ->
            model

        _ ->
            let status = if model.password == model.passwordConfirmation
                         then Ok
                         else Error PasswordMismatch
            in { model | status = status }
        

checkUsername : String -> Cmd Msg
checkUsername =
    getRequest >> Http.send CheckUsernameResult

getRequest : String -> Http.Request String
getRequest username =
    let url = "/usernames/" ++ username
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
    let errorDiv = case model.status of
                       Ok ->
                           div [ class "alert alert-danger hide" ] []
                       Checking ->
                           div [ class "alert alert-warning"]
                               [ text "checking username" ]
                       Error UsernameTaken ->
                           div [ class "alert alert-danger" ]
                               [ text "username is taken" ]
                       Error PasswordMismatch ->
                           div [ class "alert alert-danger" ]
                               [ text "passwords don't match" ]
        inputs = viewFormInputs model
    in 
        Html.form [] (errorDiv :: inputs)

viewFormInputs : Model -> List (Html Msg)
viewFormInputs {username, password, passwordConfirmation} =
    [ viewFormInput "User Name" "text" username Username
    , viewFormInput "Password" "password" password Password
    , viewFormInput "Password Confirmation" "password" passwordConfirmation PasswordConfirmation
    ]
    
    
viewFormInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewFormInput labelText inputType inputValue handler =
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , input [ type_ inputType
                , onInput handler
                , class "form-control"
                , value inputValue
                ] []
        ]
