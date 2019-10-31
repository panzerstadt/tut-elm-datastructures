module Index exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Maybe exposing (..)



-- the main export (export default)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- defaultProps
-- what type is it? what is the structure
-- records as a model
-- the only way we can change the structure is on definition.
-- after that, all changes can only be values that the records hold
-- ensure that your data doesn't suddenly look different after
-- going through a function.


type alias Model =
    { email : Bool
    , autoplay : Autoplay
    , location : Bool
    }


type Autoplay
    = Off
    | On { bluetooth : Bool, wifi : Bool, audio : Bool }


init : Model
init =
    Model False Off False



-- update
-- what do we want our interactions with the model to be like??


type Msg
    = ToggleEmail Bool
    | ToggleAutoplay Bool
    | ToggleLocation Bool


handleAutoplay autoplay model =
    case autoplay of
        Off ->
            { model | autoplay = Off }

        On _ ->
            { model | autoplay = On { bluetooth = False, wifi = False, audio = False } }


update : Msg -> Model -> Model
update onEvent model =
    case onEvent of
        ToggleEmail clickEvent ->
            { model | email = not model.email }

        ToggleAutoplay clickEvent ->
            { model | autoplay = Off }

        ToggleLocation clickEvent ->
            { model | location = not model.location }


modelStyle : List (Attribute msg)
modelStyle =
    [ style "display" "flex"
    , style "justify-content" "space-evenly"
    , style "align-items" "center"
    , style "height" "100vh"
    , style "font-family" "arial"
    ]


view : Model -> Html Msg
view model =
    div modelStyle
        [ div []
            [ h2 [ style "padding-left" "15px" ] [ text "View" ]
            , viewInput "checkbox" "Email Notifications" model.email ToggleEmail
            , viewInput "checkbox" "Autoplay" model.autoplay ToggleAutoplay
            , viewInput "checkbox" "Location" model.location ToggleLocation
            ]
        , div []
            [ h2 [ style "padding-left" "10px" ] [ text "Data Model" ]
            , viewData model
            ]
        ]


viewAutoplay : Bool -> Model -> Html Msg
viewAutoplay checked model =
    case checked of
        False ->
            viewInput "checkbox" "Autoplay" model.autoplay ToggleAutoplay

        True ->
            div []
                [ viewInput "checkbox" "Autoplay" model.autoplay ToggleAutoplay
                , div []
                    [ viewInput "checkbox" "Audio" model.autoplay.on.audio ToggleAutoplay
                    , viewInput "checkbox" "Bluetooth" model.autoplay.on.bluetooth ToggleAutoplay
                    , viewInput "checkbox" "Wifi" model.autoplay.on.wifi ToggleAutoplay
                    ]
                ]


inputStyle : List (Attribute msg)
inputStyle =
    [ style "padding" "5px 10px"
    ]


viewInput : String -> String -> Bool -> (Bool -> msg) -> Html msg
viewInput t l d toMsg =
    div inputStyle
        [ input [ type_ t, default d, onCheck toMsg ] []
        , label [] [ text l ]
        ]


boolToString : Bool -> String
boolToString boolean =
    if boolean then
        "true"

    else
        "false"


dataStyle : List (Attribute msg)
dataStyle =
    [ style "background-color" "#f2f2f2"
    , style "padding" "5px 10px"
    , style "width" "300px"
    ]


viewData : Model -> Html msg
viewData model =
    div [ style "margin-top" "10px" ]
        [ div ([ style "color" "blue" ] ++ dataStyle) [ text ("Email" ++ " : " ++ boolToString model.email) ]
        , div ([ style "color" "orange" ] ++ dataStyle) [ text ("Autoplay" ++ " : " ++ boolToString model.autoplay) ]
        , div ([ style "color" "green" ] ++ dataStyle) [ text ("Location" ++ " : " ++ boolToString model.location) ]
        ]
