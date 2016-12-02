import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main : Program Never Model Msg
main = beginnerProgram
       { model = model
       , view = view
       , update = update
       }

type alias Model =
    { username : String
    , password : String
    , passwordConfirmation : String
    , error : Error
    }

type Error = NoError | PasswordMismatch

model : Model
model = 
    validate { username = "Bob"
             , password = "s3kr3t"
             , passwordConfirmation = "asdf"
             , error = NoError
             }

type Msg = Username String
         | Password String
         | PasswordConfirmation String

update : Msg -> Model -> Model
update msg model =
    let updated = case msg of
        Username username ->
            { model | username = username }
        Password password ->
            { model | password = password }
        PasswordConfirmation passwordConfirmation ->
            { model | passwordConfirmation = passwordConfirmation }
    in validate updated

validate : Model -> Model
validate model =
    let error = if model.password == model.passwordConfirmation
                then NoError
                else PasswordMismatch
    in { model | error = error }

view : Model -> Html Msg
view model =
    let errorDiv = case model.error of
                       NoError ->
                           div [ class "alert alert-danger hide" ] []
                       PasswordMismatch ->
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
