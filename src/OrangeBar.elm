module OrangeBar exposing (barchart)

import Html exposing (Attribute, Html)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Types as ST
import Axis
import TypedSvg.Types exposing (Transform(..))
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg.Core exposing (Svg)
import Color exposing (Color)

colourA : Color
colourA = Color.rgb255 26 255 26

colourB : Color
colourB = Color.rgb255 75 0 146

attrib : List (Attribute a)
attrib = 
    [ SA.height <| ST.px 220
    , SA.width <| ST.px 220
    ]

padding : Float
padding = 10

h : Float
h = 200

w : Float
w = 200

getLabs : String -> String -> Int -> String
getLabs l1 l2 i =
    case i of
        1 -> l1
        2 -> l2
        _ -> "Error"

xScale : BandScale Int
xScale =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding ) [1, 2]

xAxis : String -> String -> Svg msg
xAxis l1 l2=
    Axis.bottom [] (Scale.toRenderable (getLabs l1 l2) xScale)

yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 18 )

yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale

getcolour : Int -> Color
getcolour i =
    case i of
        1 -> colourA
        2 -> colourB
        _ -> Color.black


type alias BarRecord = 
    { val : Float
    , pos : Int
    , lab : String
    }

drawbar : BarRecord -> Svg msg
drawbar val =
    S.g [ SA.class ["bar"] ] 
        [ S.rect 
            [ SA.x <| ST.px (Scale.convert xScale val.pos)
            , SA.y <| ST.px (Scale.convert yScale val.val)
            , SA.width <| ST.px <| Scale.bandwidth xScale
            , SA.height <| ST.px (h - Scale.convert yScale val.val - 2 * padding)
            , SA.fill <| ST.Paint <| getcolour val.pos
            ]
            []
        , S.text_
            [ SA.x <| ST.px <| Scale.convert xScale val.pos
            , SA.y <| ST.px <| Scale.convert yScale val.val - 5
            , SA.textAnchor ST.AnchorMiddle
            ]
            [ TypedSvg.Core.text val.lab ]]

barchart : String -> Float -> String -> Float -> Html a
barchart lab_a a lab_b b =
    S.svg attrib 
        [ S.g [ SA.transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis lab_a lab_b ]
        , S.g [ SA.transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , S.g [ SA.transform [ Translate padding padding ], SA.class [ "series" ] ] <|
            List.map drawbar [ BarRecord a 1 lab_a, BarRecord b 2 lab_b]
        ]
