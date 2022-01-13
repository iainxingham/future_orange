module Orange exposing (main, nightsPerMonth)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (padLeft)
import Round

--import OrangePie
import OrangeBar

type alias Model = 
    { intConsultants:       Int     -- Total number of consultants
    , intNoNights:          Int     -- Number of people not doing nights
    , intLeaveTime:         Int     -- Leaving time on night shift after 8pm in half hours
                                    -- eg 3 = 2130
    , floatCalls:           Float   -- Number of calls per night
    , intReturns:           Int     -- Ratio of nights with return to hospital
    , floatReturnTime:      Float   -- Time in hours when return to hospital
    , intShortDay:          Int     -- Number of consultants doing a 2PA short day at the weekend   
    , floatWeekdayRatio:    Float   -- Ratio of weekday shifts (no nights) / (nights)
                                    -- where 1 is equal numbers between both groups
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
    let
        s = (*) 52.0 <| toFloat m.intShortDay
        w = 156.0 + s
    in
    
    (/) w <| toFloat m.intConsultants

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
    let 
        -- Number of short days
        s = toFloat m.intShortDay
        -- Denominator, short days to long days (assumed fixed at 2)
        d = 2.0 + s
        -- Average PAs for weekend daytime
        p = (16.0 + (4 * s)) / d

    in
        if m.intNoNights == 0 then 0
        else p * (weekendsNoNights m) / 42

weekendPAsNights : Model -> Float
weekendPAsNights m =
    8 * (weekendsAndNights m) / 42

isModelValid : Model -> String
isModelValid m =
     if (weekendsAndNights m) > 0.0 then
        ""
     else
        "Too few people on nights rota for model to be valid"

weekendsifnonights : Model -> Float
weekendsifnonights m =
    if m.intNoNights == 0 then 0
    else weekendsNoNights m

weekdaysNights : Model -> Float
weekdaysNights m =
    let
        nn = toFloat <| (-) m.intConsultants m.intNoNights
        rnd = m.floatWeekdayRatio * (toFloat m.intNoNights)
    in
        520.0 / (nn + rnd)

weekdaysNoNights : Model -> Float
weekdaysNoNights m =
    if m.intNoNights == 0 then 0
    else m.floatWeekdayRatio * (weekdaysNights m)

weeklyOOHPANights : Model -> Float
weeklyOOHPANights m =
    let
        nights = nightPAs m
        evenings = ((weekdaysNights m) * 0.75) / 42
        weekends = weekendPAsNights m
    in
        nights + evenings + weekends

weeklyOOHPANoNights : Model -> Float
weeklyOOHPANoNights m =
    let
        evenings = ((weekdaysNoNights m) * 0.75) / 42
        weekends = weekendPAsNoNights m
    in
        evenings + weekends        

init : Model
init =
  { intConsultants = 14
  , intNoNights = 1
  , intLeaveTime = 8
  , floatCalls = 0.67
  , intReturns = 9
  , floatReturnTime = 3.0
  , intShortDay = 0
  , floatWeekdayRatio = 1
  }

type Msg
    = UpdateCons String
    | UpdateNoNights String
    | UpdateLeaveTime String
    | UpdateCalls String
    | UpdateReturns String
    | UpdateReturnTime String
    | UpdateShortDay String
    | UpdateWeekdayProp String
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
            { 
                model | floatCalls = String.toFloat new_val |> Maybe.withDefault 0.67 
            }
        UpdateReturns new_val ->
            { 
                model | intReturns = String.toInt new_val |> Maybe.withDefault 9
            }
        UpdateReturnTime new_val ->
            { 
                model | floatReturnTime = String.toFloat new_val |> Maybe.withDefault 3.0 
            }
        UpdateShortDay new_val ->
            { 
                model | intShortDay = String.toInt new_val |> Maybe.withDefault 0
            }
        UpdateWeekdayProp new_val ->
            {
                model | floatWeekdayRatio = String.toFloat new_val |> Maybe.withDefault 1.0
            }
        ResetButton ->
            init

view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "container-fluid p-5 text-white"
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
                    , div [] [
                        text "Number of weekend \"short day\" consultants"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "3"
                            , Html.Attributes.step "1"
                            , value <| String.fromInt model.intShortDay
                            , onInput UpdateShortDay
                            ] []
                        ]
                        , text <| String.fromInt model.intShortDay
                    , div [ class "container-fluid" 
                        , id "weekday_prop_slider" ]
                        [ text "Ratio of weekday evenings (no nights / nights)"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0.1"
                            , Html.Attributes.max "3"
                            , Html.Attributes.step "0.1"
                            , value <| String.fromFloat model.floatWeekdayRatio
                            , onInput UpdateWeekdayProp
                            ][]
                        , text <| String.fromFloat model.floatWeekdayRatio
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
                , div [class "container-fluid"
                    , id "controls"
                    ]
                    [
                        button [onClick ResetButton] [text "Reset"]
                    ]
                
            ] 
        , div [ class "container-fluid" 
            , id "results" ] 
            [ table [class "table table-striped"]
                [ thead [] 
                    [th [] []
                    ,th [] [text "On nights rota"]
                    ,th [] [text "Not on nights rota"]
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
                    ,td [] [text <| Round.round 2 (weekendsifnonights model)]
                    ]
                , tr []
                    [td [] [text "Nighttime weekends per year"]
                    ,td [] [text <| Round.round 2 (weekendNights model)]
                    ,td [] [text "0"]
                    ]
                , tr []
                    [td [] [text "Weekday evenings per year"]
                    ,td [] [text <| Round.round 2 (weekdaysNights model)]
                    , td [] [text <| Round.round 2 (weekdaysNoNights model)]
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
                    [td [] [text "Nights and weekends PAs (per week)"]
                    ,td [] [text <| Round.round 2 ((nightPAs model) + (weekendPAsNights model))]
                    ,td [] [text <| Round.round 2 (weekendPAsNoNights model)]
                    ]
                , tr []
                    [td [] [text "Total out of hours PAs (per week)"]
                    ,td [] [text <| Round.round 2 (weeklyOOHPANights model)]
                    ,td [] [text <| Round.round 2 (weeklyOOHPANoNights model)]
                    ]                       
                ]
                
            ] -- Add model validation here?
        , div [class "container-fluid" 
            , id "validation" ]
            [p [] [text <| isModelValid model] ]
        , div [class "container-fluid"
            , id "chart_stack"]
            [ div [class "row", id "titles" ]
                [ div [class "col-sm-4"] [ text "Daytime weekends per year" ]
                , div [class "col-sm-4"] [ text "Weekend days PAs (per week)" ]
                , div [class "col-sm-4"] [ text "Total out of hours PAs (per week)"]
                ]
            , div [class "row", id "charts" ]
                [ div [class "col-sm-4"] [ OrangeBar.barchart "Nights" 
                    (weekendsAndNights model) 
                    "No nights"
                    (weekendsifnonights model) ]
                , div [class "col-sm-4"] [ OrangeBar.barchart "Nights"
                    (weekendPAsNights model)
                    "No nights"
                    (weekendPAsNoNights model) ]
                , div [class "col-sm-4"] [ OrangeBar.barchart "Nights"
                    (weeklyOOHPANights model)
                    "No nights"
                    (weeklyOOHPANoNights model) ]
                ]
            ]
        ] 
        

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }