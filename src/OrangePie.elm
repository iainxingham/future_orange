{-- Adapted from https://elm-visualization.netlify.app/piechart/ --}

module OrangePie exposing (piechart)

import TypedSvg exposing (g, svg, text_)
import Shape exposing (defaultPieConfig)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)
import Array exposing (Array)
import Color exposing (Color)
import Path
import Html exposing (Attribute)
import Html exposing (Html)

w : Float
w = 50

h : Float
h = 50

radius : Float
radius =
    min w h / 2

{-- Accessible (eg for colour blind) palette from https://davidmathlogic.com/colorblind/ --}
colours : Array Color
colours =
    Array.fromList
        [ Color.rgb255 26 255 26
        , Color.rgb255 75 0 146
        ]

pieSlice : Int -> Shape.Arc -> Svg msg
pieSlice index datum =
    Path.element (Shape.arc datum) [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colours, stroke <| Paint Color.white ]

pieLabel : Shape.Arc -> ( String, Float ) -> Svg msg
pieLabel slice ( label, _ ) =
    let
        ( x, y ) =
            Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }
    in
    text_
        [ transform [ Translate x y ]
        , dy (em 0.35)
        , textAnchor AnchorMiddle
        ]
        [ text label ]

piechart : String -> Float -> String -> Float -> List(Attribute a) -> Html.Html a
piechart lab_a a lab_b b attrib = 
    let
        pieData =
            Shape.pie { defaultPieConfig | outerRadius = radius } [a, b]
    in
    svg attrib
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap pieSlice pieData
            , g [] <| List.map2 pieLabel pieData [(lab_a, a), (lab_b, b)]
            ]
        ]