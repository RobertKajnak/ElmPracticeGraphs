-- Show an analog clock for your time zone.
--
-- Dependencies:
--   elm install elm/svg
--   elm install elm/time
--   elm install ryannhg/date-format
--   elm install terezka/line-charts
--
-- For a simpler version, check out:
--   https://elm-lang.org/examples/time
--
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg 
import Svg.Attributes as Svga
import Task
import Time
import Set

import DateFormat

import File exposing (File)
import File.Select as Select
import File.Download as Download

import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots
-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , content : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
    Nothing
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )

dateFormatterHu : Time.Zone -> Time.Posix -> String
dateFormatterHu =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.text "."
        , DateFormat.monthNumber
        , DateFormat.text "."
        , DateFormat.dayOfMonthNumber
        ]


-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | CsvRequested
  | CsvSelected File
  | CsvLoaded String
  | CsvDownload



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )
      
    CsvRequested ->
      ( model
      , Select.file ["text/csv"] CsvSelected
      )

    CsvSelected file ->
      ( model
      , Task.perform CsvLoaded (File.toString file)
      )

    CsvLoaded content ->
      ( { model | content = Just content }
      , Cmd.none
      )

    CsvDownload ->
      ( model
      , downloadModel model
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
  div []
  [
    p [ style "white-space" "pre" ] [ text (modelContentString model)]
    , chart model.content
    , button [ onClick CsvRequested ] [ text "Load CSV" ]
    , getDownloadButton model
    , p [] [text "The Current Date is: "]
    , p [] [text (getNiceDate model.time model.zone)]
    , p [] [text "The current Time is: "]
    , p [] [text (String.fromInt (Time.posixToMillis model.time))]
    ,
      let
        hour   = toFloat (Time.toHour   model.zone model.time)
        minute = toFloat (Time.toMinute model.zone model.time)
        second = toFloat (Time.toSecond model.zone model.time)
      in
      Svg.svg
        [ Svga.viewBox "0 0 400 400"
        , Svga.width "400"
        , Svga.height "400"
        ]
        [ Svg.circle [ Svga.cx "200", Svga.cy "200", Svga.r "120", Svga.fill "#1263a8" ] []
        , viewHand 6 60 (hour/12)
        , viewHand 6 90 (minute/60)
        , viewHand 3 90 (second/60)
        ]

  ]

modelContentString : Model -> String
modelContentString model =
  case model.content of
    Nothing ->
      "No file uploaded yet"
    Just content ->
      let
        datesList = datesInCSV model.zone content
      in
        "The data contains " ++ String.fromInt (dateCount datesList) ++
        " dates, from " ++  firstDate datesList ++
        " to " ++ lastDate datesList--yMDListToString datesList

ymdToString : Int -> String
ymdToString ymd = 
  String.join "." (List.map String.fromInt [ymd//10000, modBy 100 (ymd//100), modBy 100 ymd])

firstDate : List Int -> String
firstDate dates = 
  let 
    date = List.head dates
  in
    case date of
      Nothing ->
        "No dates Present"
      Just iDate -> 
        ymdToString iDate

lastDate : List Int -> String
lastDate dates =
  let 
    date =  List.head <| List.reverse dates
  in
    case date of
      Nothing ->
        "No dates Present"
      Just iDate -> 
        ymdToString iDate

dateCount : List Int -> Int
dateCount  dates = 
  dates
    |> Set.fromList
    |> Set.size

yMDListToString : List Int -> String
yMDListToString ymd =
  ymd 
   |> List.map String.fromInt
   |> String.join "|" 

datesInCSV : Time.Zone -> String -> List Int
datesInCSV zone mush =
  mush  -- "1,2,3\n4,5,6"
    |> contentToEntries -- ["1", "4"]
    |> List.filterMap String.toInt -- [1, 4]
    |> List.map padMillisPosix --[1000, 4000]
    |> List.map Time.millisToPosix -- [Posix 1000, Posix 4000]
    |> List.map (posixToYMD zone) -- [20200203, 20200304]

padMillisPosix : Int -> Int
padMillisPosix unpadded =
  if unpadded < 159060317100 then unpadded * 1000 else unpadded

posixToYMD :  Time.Zone -> Time.Posix -> Int
posixToYMD zone time =
  let 
    yearInt = Time.toYear zone time
    monthInt = monthToInt (Time.toMonth zone time)
    dayInt = Time.toDay zone time
  in
    (yearInt) * (if monthInt<10 then 10000 else 1000) + 
      (monthInt) * (if dayInt<10 then 1000 else 100) + 
      dayInt


monthToInt : Time.Month -> Int 
monthToInt month = 
  case month of 
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12

contentToEntries : String -> List String
contentToEntries content =
  content -- "1,2,3\n4,5,6"
    |> String.lines --["1,2,3","4,5,6"]
    |> List.map (String.split ",") --[[1,2,3],[4,5,6]]
    |> List.map List.head -- [Maybe 1, Maybe 4]
    |> List.filterMap identity -- [1, 4]



getDownloadButton : Model -> Html Msg
getDownloadButton model =
  case model.content of
    Nothing ->
      p [] []
    Just content ->
      button [ onClick CsvDownload ] [ text "Download Generated Graph"]

type alias DataPoint = 
  { date : Float
  , value : Float
  }

chart : Maybe String -> Html msg
chart contents =
  case contents of
    Nothing ->
      p [] []
    Just cont ->
      LineChart.view1 .date .value (getDataListFromCSV cont)

getDataListFromCSV : String -> List DataPoint
getDataListFromCSV contents = 
  let 
    headers = String.split "," <| Maybe.withDefault "" <| List.head <| String.lines contents 
    --values =
  in
    {-contents
      |> String.lines 
      |> List.map ( List.take 2 )
      |> List.map (List.map2 DataPoint)-}
    
    [ DataPoint 10 50 
    , DataPoint 11 60
    , DataPoint 13 70]

--getLine :  String -> List dataPoint -> LineChart.Series data
--getLine color label value = 
--  LineChart.line color Dots.diamond label value


possibleColors =
  [ Colors.red
  , Colors.blue
  , Colors.rust
  , Colors.teal
  , Colors.green
  , Colors.gold
  ]

downloadModel : Model -> Cmd msg
downloadModel model =
  case model.content of
    Nothing -> 
      Cmd.none
    Just content -> 
      Download.string "Graph.png" "image/png+jpg" content

getNiceDate : Time.Posix -> Time.Zone -> String
getNiceDate time zone =
  dateFormatterHu zone time

viewHand : Int -> Float -> Float -> Svg.Svg msg
viewHand width length turns =
  let
    t = 2 * pi * (turns - 0.25)
    x = 200 + length * cos t
    y = 200 + length * sin t
  in
  Svg.line
    [ Svga.x1 "200"
    , Svga.y1 "200"
    , Svga.x2 (String.fromFloat x)
    , Svga.y2 (String.fromFloat y)
    , Svga.stroke "white"
    , Svga.strokeWidth (String.fromInt width)
    , Svga.strokeLinecap "round"
    ]
    []