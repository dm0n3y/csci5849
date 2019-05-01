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

import Dict exposing (Dict)


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
firstCard = Card One Red Oval Solid

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
    String.concat ["./img/", cardToString card, ".png"]

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

type Player = X | Y

type alias Model =
    { deck: List Card
    , table: Dict Int Card
    , currentTime: Time.Posix
    , setTime: Int
    , xPoints: Int
    , yPoints: Int
    , xSet: List Int
    , ySet: List Int
    , xStartTime: Maybe Time.Posix
    , yStartTime: Maybe Time.Posix
    , gameOver: Bool
    }

init : ( Model, Cmd Msg )
init =
    (
    { deck = []
    , table = Dict.fromList []
    , currentTime = Time.millisToPosix 0
    , setTime = 5000
    , xPoints = 0
    , yPoints = 0
    , xSet = []
    , ySet = []
    , xStartTime = Nothing
    , yStartTime = Nothing
    , gameOver = False
    }
    , message Shuffle
    )

numbersValid cd1 cd2 cd3 =
    let
        c1 = cd1.number
        c2 = cd2.number
        c3 = cd3.number
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)

colorsValid cd1 cd2 cd3 =
    let
        c1 = cd1.color
        c2 = cd2.color
        c3 = cd3.color
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)

shapesValid cd1 cd2 cd3 =
    let
        c2 = cd2.shape
        c1 = cd1.shape
        c3 = cd3.shape
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)

fillsValid cd1 cd2 cd3 =
    let
        c1 = cd1.fill
        c2 = cd2.fill
        c3 = cd3.fill
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)

type CheckResult = Valid | Invalid | Incomplete

checkSet : List Card -> CheckResult
checkSet set =
    case set of
        [] -> Incomplete
        [_] -> Incomplete
        [_, _] -> Incomplete
        c1::c2::c3::_ ->
            if (numbersValid c1 c2 c3 && colorsValid c1 c2 c3 && shapesValid c1 c2 c3 && fillsValid c1 c2 c3) then
                Valid
            else
                Invalid

---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick

---- UPDATE ----

type Msg
    = Tick Time.Posix
    | StartSet Player
    | StartSetTime Player Time.Posix
    | ClearSet Player
    | Shuffle
    | Shuffled (List Card)
    | AddToSet Player Int
    | RemoveFromSet Player Int
    | TakeSet Player
    | GameOver

remainingTimeHelper startSetTime currentTime setTime =
    case startSetTime of
        Nothing -> Nothing
        Just startTime -> Just (setTime - (Time.posixToMillis currentTime - Time.posixToMillis startTime))
xRemainingTime : Model -> Maybe Int
xRemainingTime { xStartTime, currentTime, setTime } = remainingTimeHelper xStartTime currentTime setTime
yRemainingTime : Model -> Maybe Int
yRemainingTime { yStartTime, currentTime, setTime } = remainingTimeHelper yStartTime currentTime setTime

xSetCards : Model -> List Card
xSetCards { xSet, table } =
    xSet
    |> List.map(\i ->
        case Dict.get i table of
            Nothing -> firstCard  --- HACK ---
            Just cd -> cd
        )

ySetCards : Model -> List Card
ySetCards { ySet, table } =
    ySet
    |> List.map(\i ->
        case Dict.get i table of
            Nothing -> firstCard  --- HACK ---
            Just cd -> cd
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            (
                { model | currentTime = newTime },
                case remainingTimeHelper model.xStartTime newTime model.setTime of
                    Just remaining -> if remaining > 0 then Cmd.none else message (ClearSet X)
                    Nothing ->
                        case remainingTimeHelper model.yStartTime newTime model.setTime of
                            Just remaining -> if remaining > 0 then Cmd.none else message (ClearSet Y)
                            Nothing -> Cmd.none
            )
        Shuffle -> ( model, generate Shuffled (shuffle cards) )
        Shuffled deck ->
            let
                table =
                    List.take 12 deck
                    |> List.indexedMap (\i cd -> (i, cd))
                    |> Dict.fromList
                remainingDeck = List.drop 12 deck
            in
            ({ model | table = table, deck = remainingDeck }, Cmd.none)
        StartSet p -> ( model, Time.now |> Task.perform (StartSetTime p) )
        StartSetTime p time ->
            case p of
                X -> ( { model | xStartTime = Just time }, Cmd.none )
                Y -> ( { model | yStartTime = Just time }, Cmd.none )
        ClearSet p ->
            case p of
                X -> ( { model | xStartTime = Nothing, xSet = [] }, Cmd.none )
                Y -> ( { model | yStartTime = Nothing, ySet = [] }, Cmd.none )
        AddToSet p i ->
            case p of
                X ->
                    let
                        newModel = { model | xSet = i :: model.xSet }
                    in
                    case checkSet (xSetCards newModel) of
                        Incomplete -> ( newModel, Cmd.none )
                        Invalid -> ( { newModel | xPoints = model.xPoints - 1 }, message (ClearSet X) )
                        Valid -> ( newModel, message (TakeSet X) )
                Y ->
                    let
                        newModel = { model | ySet = i :: model.ySet }
                    in
                    case checkSet (ySetCards newModel) of
                        Incomplete -> ( newModel, Cmd.none )
                        Invalid -> ( { newModel | yPoints = model.yPoints - 1 }, message (ClearSet Y) )
                        Valid -> ( newModel, message (TakeSet Y) )
        RemoveFromSet p i ->
            case p of
                X ->
                    (
                       { model | xSet = model.xSet |> List.filter(\cd -> cd /= i) },
                        Cmd.none
                    )
                Y ->
                    (
                       { model | ySet = model.ySet |> List.filter(\cd -> cd /= i) },
                        Cmd.none
                    )
        TakeSet p ->
            case p of
                X ->
                    let newCards = List.take 3 model.deck in
                    if List.length newCards == 0 then
                        ({ model | xPoints = model.xPoints + 1 }, message GameOver)
                    else
                        let newDeck = List.drop 3 model.deck in
                        let newIndexedCards = List.map2 Tuple.pair model.xSet newCards |> Dict.fromList in
                        let newTable = Dict.union newIndexedCards model.table in
                        let newPoints = model.xPoints + 1 in
                        ({ model | deck = newDeck, table = newTable, xPoints = newPoints }, message (ClearSet X))
                Y ->
                    let newCards = List.take 3 model.deck in
                    if List.length newCards == 0 then
                        ({ model | yPoints = model.yPoints + 1 }, message GameOver)
                    else
                        let newDeck = List.drop 3 model.deck in
                        let newIndexedCards = List.map2 Tuple.pair model.ySet newCards |> Dict.fromList in
                        let newTable = Dict.union newIndexedCards model.table in
                        let newPoints = model.yPoints + 1 in
                        ({ model | deck = newDeck, table = newTable, yPoints = newPoints }, message (ClearSet Y))
        GameOver -> ({ model | gameOver = True }, Cmd.none)


---- VIEW ----

view : Model -> Html Msg
view (
    { deck
    , table
    , xSet
    , ySet
    , xStartTime
    , yStartTime
    , currentTime
    , setTime
    , gameOver
    } as model) =
    let
        cardAttributes : Int -> Card -> List (Attribute Msg)
        cardAttributes i cd =
            case xRemainingTime model of
                Just time ->
                    if List.member i xSet then
                        [class "card", class "set", onClick (RemoveFromSet X i)]
                    else
                        [class "card", onClick (AddToSet X i)]
                Nothing ->
                    case yRemainingTime model of
                        Just time ->
                            if List.member i ySet then
                                [class "card", class "set", onClick (RemoveFromSet Y i)]
                            else
                                [class "card", onClick (AddToSet Y i)]
                        Nothing ->
                            [class "card"]

        tableCards =
            table
            |> Dict.toList
            |> List.map (
                \(i, cd) ->
                    div (cardAttributes i cd)
                        [img [ src (cardImgPath cd) ] []]
            )
        tableView = div [id "table"] tableCards

        xRemainingTimeView =
            div [id "x-remaining-time"]
                (
                    case xRemainingTime model of
                        Nothing -> []
                        Just time -> [text (String.concat ["X: ", String.fromInt time])]
                )
        yRemainingTimeView =
            div [id "y-remaining-time"]
                (
                    case yRemainingTime model of
                        Nothing -> []
                        Just time -> [text (String.concat ["Y: ", String.fromInt time])]
                )

        containerAttributes =
            case (gameOver, xStartTime, yStartTime) of
                (True, _, _) -> [id "container", class "game-over"]
                (_, Nothing, Nothing) -> [id "container", onClick (StartSet X)]
                (_, _, _) -> [id "container"]
        containerNodes =
            if gameOver then [text "GAME OVER"] else [ tableView , xRemainingTimeView , yRemainingTimeView ]
    in
    div containerAttributes containerNodes

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
