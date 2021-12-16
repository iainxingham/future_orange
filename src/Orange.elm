module Orange exposing (main, nightsPerMonth)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (padLeft) 
import Html.Events exposing (onClick)
import Round exposing (round)

type alias Model = 
    { intConsultants: Int     -- Total number of consultants
    , intNoNights:    Int     -- Number of people not doing nights
    , intLeaveTime:   Int     -- Leaving time on night shift after 8pm in half hours
                              -- eg 3 = 2130
    , floatCalls:     Float   -- Number of calls per night
    , intReturns:     Int     -- Ratio of nights with return to hospital
    , floatReturnTime: Float   -- Time in hours when return to hospital        
    }

convertLeaveTime : Int -> String
convertLeaveTime x =
    let midnight y = 
            if y < 24 then y
            else y - 24

        minutes = remainderBy 2 x
            |> (*) 30
            |> String.fromInt
            |> padLeft 2 '0'

        hours = x // 2
            |> (+) 20
            |> midnight
            |> String.fromInt
            |> padLeft 2 '0'

    in
    hours ++ ":" ++ minutes

nightsPerMonth : Model -> Float
nightsPerMonth m =
    let n = toFloat (m.intConsultants - m.intNoNights)

    in 
    365.25 / (12.0 * n)

callinsPerYear : Model -> Float
callinsPerYear m = 
    let n = toFloat (m.intConsultants - m.intNoNights)
        c = toFloat m.intReturns

    in 
    365.25 / (n * c) 

weekendsNoNights : Model -> Float
weekendsNoNights m = 
    (/) 156.0 <| toFloat m.intConsultants

weekendNights : Model -> Float
weekendNights m = 
    (/) 52.0 <| toFloat (m.intConsultants - m.intNoNights)

weekendsAndNights : Model -> Float
weekendsAndNights m =
    (weekendsNoNights m) - (weekendNights m)

nightPAs : Model -> Float
nightPAs m = 
    let
        evening = (toFloat m.intLeaveTime) / 6
        calls = m.floatCalls / 6
        return = m.floatReturnTime / (3 * (toFloat m.intReturns))
        each = 365.25 / (toFloat (m.intConsultants - m.intNoNights))
    in
    
        (evening + calls + return) * each / 42

weekendPAsNoNights : Model -> Float
weekendPAsNoNights m = 
    8 * (weekendsNoNights m) / 42

weekendPAsNights : Model -> Float
weekendPAsNights m =
    8 * (weekendsAndNights m) / 42

init : Model
init =
  { intConsultants = 14
  , intNoNights = 1
  , intLeaveTime = 8
  , floatCalls = 0.67
  , intReturns = 9
  , floatReturnTime = 3.0
  }

type Msg
    = UpdateCons String
    | UpdateNoNights String
    | UpdateLeaveTime String
    | UpdateCalls String
    | UpdateReturns String
    | UpdateReturnTime String
    | ResetButton

update : Msg -> Model -> Model
update msg model = 
    case msg of
        UpdateCons new_val ->
            { model | intConsultants = String.toInt new_val |> Maybe.withDefault 0
            }
        UpdateNoNights new_val ->
            { model | intNoNights = String.toInt new_val |> Maybe.withDefault 0 
            }
        UpdateLeaveTime new_val ->
            { model | intLeaveTime = String.toInt new_val |> Maybe.withDefault 0
            }
        UpdateCalls new_val ->
            { model | floatCalls = String.toFloat new_val |> Maybe.withDefault 0.67 
            }
        UpdateReturns new_val ->
            { model | intReturns = String.toInt new_val |> Maybe.withDefault 9
            }
        UpdateReturnTime new_val ->
            { model | floatReturnTime = String.toFloat new_val |> Maybe.withDefault 3.0 
            }
        ResetButton ->
            { intConsultants = 14
            , intNoNights = 1
            , intLeaveTime = 8
            , floatCalls = 0.67
            , intReturns = 9
            , floatReturnTime = 3.0
            }

view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "container-fluid p-5 bg-primary text-white"
            , id  "title_box" 
            ]
            [ p [ ] [ h1 [ ] [text "Future is Orange"] ]
            , p [ ] [text "Adjust the number of people on the rota and the number doing nights to see the effects on individual time commitments"]
            ]
        , div [ class "container-fluid" ]
            [ div [ class "row"] 
                [ div [ class "col"
                    , id "main_params" 
                    ]
                    [ div [ class "container-fluid" 
                        , id "consultants_slider" ]
                        [ text "Total number of people on the rota:"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "10"
                            , Html.Attributes.max "25"
                            , value <| String.fromInt model.intConsultants
                            , onInput UpdateCons
                            ]
                            []
                        , text <| String.fromInt model.intConsultants
                        ]
                    , div [ class "container-fluid" 
                        , id "nonights_slider" ]
                        [ text "Number of people not doing nights:"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "9"
                            , value <| String.fromInt model.intNoNights
                            , onInput UpdateNoNights
                            ]
                            []
                        , text <| String.fromInt model.intNoNights
                        ]      
                    ]
                , div [class "col"
                    , id "other_params"
                    ]
                    [ div [] [
                        text "Typical leaving time (night shift):"
                        , input
                            [type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "16"
                            , value <| String.fromInt model.intLeaveTime
                            , onInput UpdateLeaveTime 
                            ] []
                        ]
                        , text <| convertLeaveTime model.intLeaveTime
                    , div [] [
                        text "Average number of calls per night:"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "5"
                            , Html.Attributes.step "0.1"
                            , value <| String.fromFloat model.floatCalls
                            , onInput UpdateCalls
                            ] []
                        ]
                        , text <| String.fromFloat model.floatCalls
                    , div [] [
                        text "Return to hospital every x nights:"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "21"
                            , value <| String.fromInt model.intReturns
                            , onInput UpdateReturns
                            ] []
                        ]
                        , text <| String.fromInt model.intReturns
                    , div [] [
                        text "Typical number of hours present on emergency return:"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0.5"
                            , Html.Attributes.max "5"
                            , Html.Attributes.step "0.5"
                            , value <| String.fromFloat model.floatReturnTime
                            , onInput UpdateReturnTime
                            ] []
                        ]
                        , text <| String.fromFloat model.floatReturnTime
                    ]
                ]
                , div [class "col"
                    , id "controls"
                    ]
                    [
                        button [onClick ResetButton] [text "Reset"]
                    ]
                
            ] 
        , div [ class "container mt-5" 
            , id "results" ] 
            [ table []
                [ thead [] 
                    [th [] []
                    ,th [] [text "On nights rota"]
                    ,th [] [text "No nights"]
                    ]
                , tr []
                    [td [] [text "Number of nights per month"]
                    ,td [] [text <| Round.round 2 (nightsPerMonth model)]
                    ,td [] [text "0"]
                    ]
                , tr []
                    [td [] [text "Approximate number of overnight call ins per year"]
                    ,td [] [text <| Round.round 2 (callinsPerYear model)]
                    ,td [] [text "0"]
                    ]
                , tr []
                    [td [] [text "Daytime weekends per year"]
                    ,td [] [text <| Round.round 2 (weekendsAndNights model)]
                    ,td [] [text <| Round.round 2 (weekendsNoNights model)]
                    ]
                , tr []
                    [td [] [text "Nighttime weekends per year"]
                    ,td [] [text <| Round.round 2 (weekendNights model)]
                    ,td [] [text "0"]
                    ]
                , tr []
                    [td [] [text "Night shift PAs (per week)"]
                    ,td [] [text <| Round.round 2 (nightPAs model)]
                    ,td [] [text "0"]
                    ]
                , tr []
                    [td [] [text "Weekend days PAs (per week)"]
                    ,td [] [text <| Round.round 2 (weekendPAsNights model)]
                    ,td [] [text <| Round.round 2 (weekendPAsNoNights model)]
                    ]
                , tr []
                    [td [] [text "Total out of hours PAs (per week)"]
                    ,td [] [text <| Round.round 2 ((nightPAs model) + (weekendPAsNights model))]
                    ,td [] [text <| Round.round 2 (weekendPAsNoNights model)]
                    ]                    
                ]
                
            ] -- Add model validation here?
        ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }