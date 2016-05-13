import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Date exposing (Date)

main =
  Html.program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

-- MODEL

type alias Model = Time
type alias Fraction = Float
type alias Length = Float
type alias Width = Int
type alias Hand = (Time -> Fraction, Length, Width, String)

init : (Model, Cmd Msg)
init =
  (0, Cmd.none)

-- UPDATE

type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      (newTime, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second Tick

-- VIEW

faceColour : String
faceColour =
  "#0b79ce"

handColour : String
handColour =
  "#023963"

hands : List (Hand)
hands =
  [ (hours, 20, 2, "round")
  , (minutes, 30, 2, "round")
  , (seconds, 40, 1, "square")
  ]

hours : Time -> Fraction
hours =
  timeFraction Date.hour 12

minutes : Time -> Fraction
minutes =
  timeFraction Date.minute 60

seconds : Time -> Fraction
seconds =
  timeFraction Date.second 60

timeFraction : (Date -> Int) -> Int -> Time -> Fraction
timeFraction fn max time =
  fn (Date.fromTime time) |> fraction max

fraction : Int -> Int -> Fraction
fraction max value =
  (toFloat (value % max)) / (toFloat max)

handPosition : (Time -> Fraction) -> Length -> Time -> (String, String)
handPosition fn length time =
  let
    angle =
      turns ((fn time) - 0.25)
    handX =
      toString (50 + length * cos angle)
    handY =
      toString (50 + length * sin angle)
  in
    (handX, handY)

makeHand : Model -> Hand -> Svg a
makeHand  model (fn, length, width, cap) =
  let
    (x, y) = handPosition fn length model
  in
    line [ x1 "50"
         , y1 "50"
         , x2 x
         , y2 y
         , stroke handColour
         , strokeWidth (toString width)
         , strokeLinecap cap
         ] []

view : Model -> Html Msg
view model =
  let
    svgHands = List.map (makeHand model) hands
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
        (circle [ cx "50"
                , cy "50"
                , r "45"
                , fill faceColour
                ] [] :: svgHands)
