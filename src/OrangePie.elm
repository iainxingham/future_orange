{-- Adapted from https://elm-visualization.netlify.app/piechart/ --}

module OrangePie exposing (piechart)

import TypedSvg as S -- exposing (g, svg, text_, rect)
import TypedSvg.Attributes as SA
import TypedSvg.Types as ST
import Shape exposing (defaultPieConfig)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, style)
--import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), percent)
import Array exposing (Array)
import Color exposing (Color)
import Path
import Html exposing (Attribute)
import Html exposing (Html)
import TypedSvg.Core exposing (attribute)

w : Float
w = 200

h : Float
h = 200

radius : Float
radius =
    min w h / 2

-- Gap between pie chart and legend
leg_gap : Float
leg_gap = 10

-- Length of side of legend square
leg_size : Float
leg_size = 15

{-- Accessible (eg for colour blind) palette from https://davidmathlogic.com/colorblind/ --}
colours : Array Color
colours =
    Array.fromList
        [ Color.rgb255 26 255 26
        , Color.rgb255 75 0 146
        ]

colourA : Color
colourA = Color.rgb255 26 255 26

colourB : Color
colourB = Color.rgb255 75 0 146

{-- drawsquare : Float -> Float -> Float -> Svg msg
drawsquare x y size =
    S.g [ SA.class [ "column" ] ]
        [ S.rect
            [ SA.x <| ST.px x
            , SA.y <| ST.px y
            , SA.width <| ST.px size
            , SA.height <| ST.px size
            ]
            []
        , S.text_
            [ SA.x <| ST.px x
            , SA.y <| ST.px y
            , SA.textAnchor ST.AnchorMiddle
            ]
            [ text "Random label" ]
        ]

draw_legend_rect : Float -> String -> Svg msg
draw_legend_rect h_off style = 
    S.rect [ SA.x <| ST.px (w + leg_gap)
           , SA.y <| ST.px h_off
           , SA.width <| ST.px (w + leg_gap + leg_size)
           , SA.height <| ST.px (h_off + leg_size)
           , SA.rx <| ST.px 5
           , SA.style style
           ][] --}

pieSlice : Int -> Shape.Arc -> Svg msg
pieSlice index datum =
    Path.element (Shape.arc datum) [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colours, stroke <| Paint Color.white ]

pieLabel : Shape.Arc -> ( String, Float ) -> Svg msg
pieLabel slice ( label, _ ) =
    let
        ( x, y ) =
            Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }
    in
    S.text_
        [ transform [ Translate x y ]
        , dy (percent 5)                            -- dy (em 0.35)
        , textAnchor AnchorMiddle
        --, ST.class ["labs"]
        ]
        [ text label ]

attrib : List (Attribute a)
attrib = 
    [ SA.height <| ST.px 200
    , SA.width <| ST.px 200
    ]

piechart : String -> Float -> String -> Float -> Html a
piechart lab_a a lab_b b = 
    let
        pieData =
            Shape.pie { defaultPieConfig | outerRadius = radius } [a, b]
    in
    S.svg attrib
        [ S.g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ S.g [] <| List.indexedMap pieSlice pieData
            , S.g [] <| List.map2 pieLabel pieData [(lab_a, a), (lab_b, b)]
            ]
        {--, S.g [ transform [ Translate (w + leg_gap ) (h / 4) ] ]
            [ S.g [] [ draw_legend_rect 0 "fill:rgb(26,255,26)"
                     , draw_legend_rect 30 "fill:rgb(75,0,146)"
                     ]
            ]--} 
        ]