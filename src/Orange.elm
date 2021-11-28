module Orange exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

type alias Model = 
    { intConsultants: Int -- Total number of consultants
    , intNoNights:    Int -- Number of people not doing nights
    }

init : Model
init =
  { intConsultants = 14
  , intNoNights = 1
  }

type Msg
    = UpdateCons String
    | UpdateNoNights String

update : Msg -> Model -> Model
update msg model = 
    case msg of
        UpdateCons new_val ->
            { intConsultants = String.toInt new_val |> Maybe.withDefault 0
            , intNoNights = model.intNoNights
            }
        UpdateNoNights new_val ->
            { intConsultants = model.intConsultants
            , intNoNights = String.toInt new_val |> Maybe.withDefault 0 }

view : Model -> Html Msg
view model =
    div [ id "set_params" ]
        [ div [ class "container" 
            , id "consultants_slider" ]
            [ text "Total number of people on the rota:"
            , input
                [ type_ "range"
                , Html.Attributes.min "5"
                , Html.Attributes.max "25"
                , value <| String.fromInt model.intConsultants
                , onInput UpdateCons
                ]
                []
            , text <| String.fromInt model.intConsultants
            ]
        , div [ class "container" 
            , id "nonights_slider" ]
            [ text "Number of people not doing nights:"
            , input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "10"
                , value <| String.fromInt model.intNoNights
                , onInput UpdateNoNights
                ]
                []
            , text <| String.fromInt model.intNoNights
            ]      
        ]
    

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }