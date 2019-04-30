module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

import Task.Extra exposing (message)

import Random exposing (Seed, generate)
import Random.List exposing (shuffle)

import Time


numberToString : Number -> String
numberToString n =
    case n of
        One -> "1"
        Two -> "2"
        Three -> "3"

colorToString : Color -> String
colorToString c =
    case c of
        Red -> "red"
        Purple -> "purple"
        Green -> "green"

shapeToString : Shape -> String
shapeToString s =
    case s of
        Oval -> "oval"
        Squiggle -> "squiggle"
        Diamond -> "diamond"

fillToString : Fill -> String
fillToString f =
    case f of
        Solid -> "filled"
        Striped -> "shaded"
        Empty -> "empty"

---- MODEL ----

numberIterator : Maybe Number -> Maybe Number
numberIterator n =
    case n of
        Nothing -> Just One
        Just One -> Just Two
        Just Two -> Just Three
        Just Three -> Nothing
firstNumber = One

colorIterator : Maybe Color -> Maybe Color
colorIterator c =
    case c of
        Nothing -> Just Red
        Just Red -> Just Purple
        Just Purple -> Just Green
        Just Green -> Nothing
firstColor = Red

shapeIterator : Maybe Shape -> Maybe Shape
shapeIterator s =
    case s of
        Nothing -> Just Oval
        Just Oval -> Just Squiggle
        Just Squiggle -> Just Diamond
        Just Diamond -> Nothing
firstShape = Oval

fillIterator : Maybe Fill -> Maybe Fill
fillIterator f =
    case f of
        Nothing -> Just Solid
        Just Solid -> Just Striped
        Just Striped -> Just Empty
        Just Empty -> Nothing
firstFill = Solid

cardIterator : Maybe Card -> Maybe Card
cardIterator card =
    case card of
        Nothing -> Just (Card One Red Oval Solid)
        Just ({ number, color, shape, fill } as cd) ->
            case numberIterator (Just number) of
                Just n ->
                    Just { cd | number = n }
                Nothing ->
                    case colorIterator (Just color) of
                        Just c ->
                            Just { cd | number = firstNumber, color = c }
                        Nothing ->
                            case shapeIterator (Just shape) of
                                Just s ->
                                    Just { cd | number = firstNumber, color = firstColor, shape = s }
                                Nothing ->
                                    case fillIterator (Just fill) of
                                        Just f ->
                                            Just { cd | number = firstNumber, color = firstColor, shape = firstShape, fill = f }
                                        Nothing ->
                                            Nothing

cards : List Card
cards =
    let
        addNextCard : List Card -> List Card
        addNextCard cardsSoFar =
            case cardsSoFar |> List.head |> cardIterator of
                Nothing -> cardsSoFar
                Just nextCard -> nextCard :: cardsSoFar |> addNextCard
    in
    addNextCard []

cardToString : Card -> String
cardToString { number, color, shape, fill } =
    String.concat [colorToString color, shapeToString shape, fillToString fill, numberToString number]

cardImgPath : Card -> String
cardImgPath card =
    String.concat ["/img/", cardToString card, ".png"]

type Number
    = One
    | Two
    | Three

type Color
    = Red
    | Purple
    | Green

type Shape
    = Oval
    | Squiggle
    | Diamond

type Fill
    = Solid
    | Striped
    | Empty

type alias Card =
    {
        number: Number,
        color: Color,
        shape: Shape,
        fill: Fill
    }

type alias Model =
    {
        deck : List Card,
        table : List Card,
        staged: List Card,
        lastSetTime: Maybe Time.Posix,
        currentTime: Time.Posix
    }

init : ( Model, Cmd Msg )
init =
    (
        { deck = [], table = [], staged = [], lastSetTime = Nothing, currentTime = Time.millisToPosix 0 },
        message Shuffle
    )

---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick

---- UPDATE ----


type Msg
    = Tick Time.Posix
    | Shuffle
    | Shuffled (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime -> ( { model | currentTime = newTime }, Cmd.none )
        Shuffle -> ( model, generate Shuffled (shuffle cards) )
        Shuffled deck ->
            let
                table = List.take 12 deck
                remainingDeck = List.drop 12 deck
            in
            ({ model | table = table, deck = remainingDeck }, Cmd.none)


---- VIEW ----


view : Model -> Html Msg
view { deck, table, currentTime } =
    let
        tableCards = table |> List.map (\cd -> div [class "card"] [img [ src (cardImgPath cd) ] []])
    in
    div [id "container"]
        tableCards

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
