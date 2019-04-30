module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Task
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
        deck: List Card,
        table: List Card,
        set: List Card,
        startSetTime: Maybe Time.Posix,
        currentTime: Time.Posix,
        setTime: Int
    }

init : ( Model, Cmd Msg )
init =
    (
        { deck = [], table = [], set = [], startSetTime = Nothing, currentTime = Time.millisToPosix 0, setTime = 5000 },
        message Shuffle
    )

---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick

---- UPDATE ----


type Msg
    = Tick Time.Posix
    | StartSetTime Time.Posix
    | ClearStartSetTime
    | Shuffle
    | Shuffled (List Card)
    | StartSet
    | AddToSet Card
    | RemoveFromSet Card

remainingTimeHelper startSetTime currentTime setTime =
    case startSetTime of
        Nothing -> Nothing
        Just startTime -> Just (setTime - (Time.posixToMillis currentTime - Time.posixToMillis startTime))

remainingTime : Model -> Maybe Int
remainingTime { startSetTime, currentTime, setTime } = remainingTimeHelper startSetTime currentTime setTime

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime -> 
            ( 
                { model | currentTime = newTime },
                case remainingTimeHelper model.startSetTime newTime model.setTime of
                    Nothing -> Cmd.none
                    Just remaining -> if remaining > 0 then Cmd.none else message ClearStartSetTime
            )
        StartSet -> ( model, Time.now |> Task.perform StartSetTime )
        StartSetTime time -> ( { model | startSetTime = Just time }, Cmd.none )
        ClearStartSetTime -> ( { model | startSetTime = Nothing }, Cmd.none )
        Shuffle -> ( model, generate Shuffled (shuffle cards) )
        Shuffled deck ->
            let
                table = List.take 12 deck
                remainingDeck = List.drop 12 deck
            in
            ({ model | table = table, deck = remainingDeck }, Cmd.none)
        AddToSet card -> ( { model | set = card :: model.set }, Cmd.none )
        RemoveFromSet card ->
            (
                { model | set = model.set |> List.filter(\cd -> cd /= card) },
                Cmd.none
            )


---- VIEW ----

view : Model -> Html Msg
view ({ deck, table, set, startSetTime, currentTime, setTime } as model) =
    let
        cardAttributes cd =
            if List.member cd set then
                [class "card", class "set", onClick (RemoveFromSet cd)]
            else
                [class "card", onClick (AddToSet cd)]

        tableCards =
            table
            |> List.map (
                \cd ->
                    div (cardAttributes cd)
                        [img [ src (cardImgPath cd) ] []]
            )
        tableView = div [id "table"] tableCards

        remainingTimeView =
            div [id "remaining-time"]
                (
                    case remainingTime model of
                        Nothing -> []
                        Just time -> [text (String.fromInt time)]
                )
    in
    div [id "container", onClick StartSet]
        [
            tableView,
            remainingTimeView
        ]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
