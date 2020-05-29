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
  {-case model.content of
    Nothing ->
      p [] []
    Just content ->-}
      button [ onClick CsvDownload ] [ text "Download Sample Data"]

type alias DataPoint = 
  { date : Float
  , value : Float
  }

strToData : String -> String -> DataPoint
strToData date value =
  DataPoint 
    (Maybe.withDefault 0 (String.toFloat date)) 
    (Maybe.withDefault 0 (String.toFloat value))

listToData: List String-> DataPoint
listToData strArr =
  case strArr of
    [d,v] ->
      strToData d v
    d :: v :: _ ->
      strToData d v
    [d] ->
      strToData d "0"
    _ ->
      DataPoint 0 0


lineFromLineData : LineData -> LineChart.Series DataPoint
lineFromLineData lineData =
  LineChart.line Colors.purple Dots.cross lineData.label lineData.data

chartArray :  List LineData -> List (LineChart.Series DataPoint) 
  -> List (LineChart.Series DataPoint) 
chartArray  data startLines =
  --case legend 
  case data of 
    [line] ->
      chartArray [] startLines
    line :: moreLines ->
      chartArray (List.drop 1 data) ((lineFromLineData line)::startLines)
    [] ->
      startLines
  {-[ LineChart.line Colors.purple Dots.cross "alma" [DataPoint 1 2]
  , LineChart.line Colors.blue Dots.square "beta" [DataPoint 4 5]
  , LineChart.line Colors.cyan Dots.circle "Chuck" [DataPoint 3 9]
  ]-}

type alias LineData = 
  {label : String
  , data : List DataPoint
  }

chart : Maybe String -> Html msg
chart contents =
  case contents of
    Nothing ->
      p [] []
    Just cont ->
      case getDataListFromCSV cont of 
        Nothing ->
          p [] []
        Just dataList ->
          LineChart.view .date .value (chartArray (getAllData cont) [])--[{label = "alma", data = [DataPoint 1 2]}]  []) 

getDataListFromCSV : String -> Maybe (List DataPoint)
getDataListFromCSV contents = 
  let 
    headers = String.split "," <| Maybe.withDefault "" <| List.head <| String.lines contents 
    --values =
  in
    contents
      |> String.lines 
      |> List.map ( String.split ",") --[[1,2,3],[4,5,6]]
      |> List.map ( List.take 2 ) -- [[1,2],[4,5]]
      |> List.map listToData
      |> List.tail
    

-- https://stackoverflow.com/questions/31932683/transpose-in-elm-without-maybe
transpose : List (List a) -> List (List a)
transpose ll =
  let heads = List.map (List.take 1) ll |> List.concat
      tails = List.map (List.drop 1) ll 
  in
      if List.length heads == List.length ll then
             heads::(transpose tails)
         else
             []


--getLine :  String -> List dataPoint -> LineChart.Series data
--getLine color label value = 
--  LineChart.line color Dots.diamond label value



getAllData : String -> List LineData
getAllData contents  =
  let
    trans = 
      contents        
        |> String.lines 
        |> List.map ( String.split ",")
        |> transpose 
  in
    List.filterMap identity <| List.indexedMap (getSingleData trans) trans


getSingleData : List (List String) -> Int -> List String -> Maybe LineData
getSingleData transposed index _  =
  if index == 0 then
    Nothing
  else
    let 
      dates = 
        transposed
          |> List.head
          |> Maybe.withDefault []
          |> List.tail
          |> Maybe.withDefault []
      goodLines =
        transposed
          |> List.drop index
          |> List.head
          |> Maybe.withDefault []

      header = Maybe.withDefault "Unknown" (List.head  goodLines)
      data = List.drop 1 goodLines
    in
      Just ( LineData header (List.map2 strToData dates data))

    
  {-let 
    decomposed = 
      contents
        |> String.lines 
        |> List.map ( String.split ",") --[[1,2,3],[4,5,6]]
        |> List.map ( List.take 2 ) -- [[1,2],[4,5]]
        |> List.map listToData
  in
    case decomposed of
      label :: rest ->
-}


possibleColors =
  [ Colors.red
  , Colors.blue
  , Colors.rust
  , Colors.teal
  , Colors.green
  , Colors.gold
  ]

sample_data = 
  """date,total,new,relearned,recapped,forgotten
1589561268000,20,3,1,16,0
1589561268000,25,12,0,10,3
1589993268000,50,16,3,27,4
1590079668000,60,15,5,33,7
1590252468000,55,13,10,28,4
1590338868000,58,14,15,18,11
1590425268000,53,10,10,26,7
1590511668000,55,20,5,25,5
1590598068000,85,16,4,60,5"""

downloadModel : Model -> Cmd msg
downloadModel model =
  {-case model.content of
    Nothing -> 
      Cmd.none
    Just content -> 
      Download.string "Graph.png" "image/png+jpg" content-}
    Download.string "Sample_data.csv" "Comma Separated Values/csv" sample_data

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