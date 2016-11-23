import Html exposing (..)
import Html.Attributes exposing (..)


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
    { username = "Bob"
    , password = "s3kr3t"
    , passwordConfirmation = "asdf"
    , error = PasswordMismatch
    }

type Msg = None

update : Msg -> Model -> Model
update msg model =
    model

view : Model -> Html Msg
view model =
    Html.form []
        (case model.error of
            NoError ->
                viewFormInputs model
            PasswordMismatch ->
                (div [ class "alert alert-danger" ]
                     [ text "passwords don't match" ]) :: viewFormInputs model)

viewFormInputs : Model -> List (Html Msg)
viewFormInputs {username, password, passwordConfirmation} =
    [ viewFormInput "User Name" "text" username
    , viewFormInput "Password" "password" password
    , viewFormInput "Password Confirmation" "password" passwordConfirmation
    ]
    
    
viewFormInput : String -> String -> String -> Html Msg
viewFormInput labelText inputType inputValue =
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , input [ type_ inputType
                , class "form-control"
                , value inputValue
                ] []
        ]
