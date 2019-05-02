module Main exposing (Card, CheckResult(..), Color(..), Deck, Fill(..), GameState(..), Milliseconds, Model, Msg(..), Number(..), PlayState(..), Player(..), PlayerRecord, SelectedTableCards(..), Shape(..), SparseTable, Table, TableCard, addToSelection, cardImgPath, cardIterator, cardToString, cards, checkSelectionForSet, colorIterator, colorToString, colorsValid, contains, decrPoints, fillIterator, fillToString, fillsValid, firstCard, firstColor, firstFill, firstNumber, firstShape, getCurrentTime, getSelectedCards, getSelectionStarted, getTableCard, getTableIndex, incrPoints, init, isSelectedBy, main, mkTableCard, numberIterator, numberToString, numbersValid, putCurrentTime, putSelectedCards, putSelectionStartedTime, remainingSelectionTime, removeFromSelection, shapeIterator, shapeToString, shapesValid, stringOfPlayer, subscriptions, update, updatePlayState, updatePlayer, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import Task
import Task.Extra exposing (message)
import Time



---- UTIL ----


numberToString : Number -> String
numberToString n =
    case n of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"


colorToString : Color -> String
colorToString c =
    case c of
        Red ->
            "red"

        Purple ->
            "purple"

        Green ->
            "green"


shapeToString : Shape -> String
shapeToString s =
    case s of
        Oval ->
            "oval"

        Squiggle ->
            "squiggle"

        Diamond ->
            "diamond"


fillToString : Fill -> String
fillToString f =
    case f of
        Solid ->
            "filled"

        Striped ->
            "shaded"

        Empty ->
            "empty"


numberIterator : Maybe Number -> Maybe Number
numberIterator n =
    case n of
        Nothing ->
            Just One

        Just One ->
            Just Two

        Just Two ->
            Just Three

        Just Three ->
            Nothing


firstNumber =
    One


colorIterator : Maybe Color -> Maybe Color
colorIterator c =
    case c of
        Nothing ->
            Just Red

        Just Red ->
            Just Purple

        Just Purple ->
            Just Green

        Just Green ->
            Nothing


firstColor =
    Red


shapeIterator : Maybe Shape -> Maybe Shape
shapeIterator s =
    case s of
        Nothing ->
            Just Oval

        Just Oval ->
            Just Squiggle

        Just Squiggle ->
            Just Diamond

        Just Diamond ->
            Nothing


firstShape =
    Oval


fillIterator : Maybe Fill -> Maybe Fill
fillIterator f =
    case f of
        Nothing ->
            Just Solid

        Just Solid ->
            Just Striped

        Just Striped ->
            Just Empty

        Just Empty ->
            Nothing


firstFill =
    Solid


cardIterator : Maybe Card -> Maybe Card
cardIterator card =
    case card of
        Nothing ->
            Just (Card One Red Oval Solid)

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


firstCard =
    Card One Red Oval Solid


cards : List Card
cards =
    let
        addNextCard : List Card -> List Card
        addNextCard cardsSoFar =
            case cardsSoFar |> List.head |> cardIterator of
                Nothing ->
                    cardsSoFar

                Just nextCard ->
                    nextCard :: cardsSoFar |> addNextCard
    in
    addNextCard []


cardToString : Card -> String
cardToString { number, color, shape, fill } =
    String.concat [ colorToString color, shapeToString shape, fillToString fill, numberToString number ]


cardImgPath : Card -> String
cardImgPath card =
    String.concat [ "./img/", cardToString card, ".png" ]


numbersValid cd1 cd2 cd3 =
    let
        c1 =
            cd1.number

        c2 =
            cd2.number

        c3 =
            cd3.number
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)


colorsValid cd1 cd2 cd3 =
    let
        c1 =
            cd1.color

        c2 =
            cd2.color

        c3 =
            cd3.color
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)


shapesValid cd1 cd2 cd3 =
    let
        c2 =
            cd2.shape

        c1 =
            cd1.shape

        c3 =
            cd3.shape
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)


fillsValid cd1 cd2 cd3 =
    let
        c1 =
            cd1.fill

        c2 =
            cd2.fill

        c3 =
            cd3.fill
    in
    (c1 == c2 && c2 == c3 && c3 == c1) || (c1 /= c2 && c2 /= c3 && c3 /= c1)


contains : TableCard -> SelectedTableCards -> Bool
contains t selection =
    case selection of
        SZero ->
            False

        SOne t1 ->
            t == t1

        STwo t1 t2 ->
            t == t1 || t == t2

        SThree t1 t2 t3 ->
            t == t1 || t == t2 || t == t3


addToSelection : TableCard -> SelectedTableCards -> SelectedTableCards
addToSelection t selection =
    case selection of
        SZero ->
            SOne t

        SOne t1 ->
            STwo t1 t

        STwo t1 t2 ->
            SThree t1 t2 t

        SThree t1 t2 t3 ->
            SThree t1 t2 t3


removeFromSelection : TableCard -> SelectedTableCards -> SelectedTableCards
removeFromSelection t selection =
    case selection of
        SZero ->
            SZero

        SOne t1 ->
            if t == t1 then
                SZero

            else
                SOne t1

        STwo t1 t2 ->
            if t == t1 then
                SOne t2

            else if t == t2 then
                SOne t1

            else
                STwo t1 t2

        SThree t1 t2 t3 ->
            if t == t1 then
                STwo t2 t3

            else if t == t2 then
                STwo t1 t3

            else if t == t3 then
                STwo t1 t2

            else
                SThree t1 t2 t3


mkTableCard : Int -> Card -> TableCard
mkTableCard i cd =
    { tableIndex = i, card = cd }


getTableCard : Int -> Table -> TableCard
getTableCard tableIndex table =
    case Dict.get tableIndex table of
        Nothing ->
            { card = firstCard, tableIndex = tableIndex }

        -- HACK --
        Just c ->
            { card = c, tableIndex = tableIndex }


getSelectedCards : Player -> SelectedTableCards
getSelectedCards player =
    case player of
        X record ->
            record.selectedCards

        Y record ->
            record.selectedCards


putSelectedCards : SelectedTableCards -> Player -> Player
putSelectedCards selectedCards player =
    case player of
        X record ->
            X { record | selectedCards = selectedCards }

        Y record ->
            Y { record | selectedCards = selectedCards }


getSelectionStarted : Player -> Maybe Time.Posix
getSelectionStarted player =
    case player of
        X { selectionStarted } ->
            selectionStarted

        Y { selectionStarted } ->
            selectionStarted


putSelectionStartedTime : Time.Posix -> Player -> Player
putSelectionStartedTime time player =
    case player of
        X record ->
            X { record | selectionStarted = Just time }

        Y record ->
            Y { record | selectionStarted = Just time }


isSelectedBy : TableCard -> Player -> Bool
isSelectedBy t player =
    case player of
        X { selectedCards } ->
            contains t selectedCards

        Y { selectedCards } ->
            contains t selectedCards


remainingSelectionTime : Player -> Time.Posix -> Milliseconds -> Maybe Milliseconds
remainingSelectionTime player currentTime selectionDuration =
    let
        remainingTime : PlayerRecord -> Maybe Milliseconds
        remainingTime { selectionStarted } =
            case selectionStarted of
                Nothing ->
                    Nothing

                Just selectionStartTime ->
                    Just <| selectionDuration - (Time.posixToMillis currentTime - Time.posixToMillis selectionStartTime)
    in
    case player of
        X record ->
            remainingTime record

        Y record ->
            remainingTime record


updatePlayState : PlayState -> Model -> Model
updatePlayState newPlayState m =
    { m | playState = newPlayState }


updatePlayer : Player -> Model -> Model
updatePlayer newPlayer m =
    case newPlayer of
        X _ ->
            { m | xPlayer = newPlayer }

        Y _ ->
            { m | yPlayer = newPlayer }


incrPoints : Player -> Player
incrPoints p =
    case p of
        X r ->
            X { r | points = r.points + 1 }

        Y r ->
            Y { r | points = r.points + 1 }


decrPoints : Player -> Player
decrPoints p =
    case p of
        X r ->
            X { r | points = r.points - 1 }

        Y r ->
            Y { r | points = r.points - 1 }


getCurrentTime : GameState -> Time.Posix
getCurrentTime gs =
    case gs of
        WithDeck currentTime _ _ ->
            currentTime

        WithEmptyDeck currentTime _ ->
            currentTime


putCurrentTime : Time.Posix -> GameState -> GameState
putCurrentTime t gs =
    case gs of
        WithDeck currentTime deck table ->
            WithDeck t deck table

        WithEmptyDeck currentTime table ->
            WithEmptyDeck t table


stringOfPlayer : Player -> String
stringOfPlayer player =
    case player of
        X _ ->
            "X"

        Y _ ->
            "Y"


getTableIndex : TableCard -> Int
getTableIndex { tableIndex } =
    tableIndex


getPoints : Player -> Int
getPoints player =
    case player of
        X { points } -> points
        Y { points } -> points



---- MODEL ----


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
    { number : Number
    , color : Color
    , shape : Shape
    , fill : Fill
    }


type alias TableCard =
    { tableIndex : Int
    , card : Card
    }


type SelectedTableCards
    = SZero
    | SOne TableCard
    | STwo TableCard TableCard
    | SThree TableCard TableCard TableCard


type alias PlayerRecord =
    { points : Int
    , selectedCards : SelectedTableCards
    , selectionStarted : Maybe Time.Posix
    }


type Player
    = X PlayerRecord
    | Y PlayerRecord


type alias Milliseconds =
    Int


type alias Deck =
    { nextDraw : ( Card, Card, Card )
    , rest : List Card
    }


type alias Table =
    Dict Int Card


type alias SparseTable =
    Dict Int (Maybe Card)


type GameState
    = WithDeck Time.Posix Deck Table
    | WithEmptyDeck Time.Posix SparseTable


type PlayState
    = BeforeFirstGame
    | PlayingGame GameState
    | GameOver


type alias Model =
    { selectionDuration : Milliseconds
    , playState : PlayState
    , xPlayer : Player
    , yPlayer : Player
    }


init : ( Model, Cmd Msg )
init =
    ( { selectionDuration = 5000
      , playState = BeforeFirstGame
      , xPlayer = X { points = 0, selectedCards = SZero, selectionStarted = Nothing }
      , yPlayer = Y { points = 0, selectedCards = SZero, selectionStarted = Nothing }
      }
    , message StartGame
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions { playState } =
    case playState of
        BeforeFirstGame ->
            Sub.none

        GameOver ->
            Sub.none

        PlayingGame _ ->
            Time.every 100 Tick



---- UPDATE ----


type Msg
    = StartGame
    | EndGame
    | NewDeck (List Card)
    | Tick Time.Posix
    | StartSelection Player
    | SetStartTime Player Time.Posix
    | Select Player TableCard
    | Deselect Player TableCard


type CheckResult
    = Valid TableCard TableCard TableCard
    | Invalid
    | Incomplete


checkSelectionForSet : SelectedTableCards -> CheckResult
checkSelectionForSet selection =
    case selection of
        SZero ->
            Incomplete

        SOne _ ->
            Incomplete

        STwo _ _ ->
            Incomplete

        SThree t1 t2 t3 ->
            let
                c1 =
                    t1.card

                c2 =
                    t2.card

                c3 =
                    t3.card
            in
            if numbersValid c1 c2 c3 && colorsValid c1 c2 c3 && shapesValid c1 c2 c3 && fillsValid c1 c2 c3 then
                Valid t1 t2 t3

            else
                Invalid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ playState, xPlayer, yPlayer, selectionDuration } as model) =
    let
        select : TableCard -> Player -> GameState -> ( Player, GameState )
        select t player gs =
            let
                selectedCards =
                    getSelectedCards player

                newSelectedCards =
                    addToSelection t selectedCards

                newPlayer =
                    putSelectedCards newSelectedCards player
            in
            case checkSelectionForSet newSelectedCards of
                Incomplete ->
                    ( newPlayer, gs )

                Invalid ->
                    ( newPlayer |> clearSelection |> decrPoints, gs )

                Valid t1 t2 t3 ->
                    let
                        newGameState =
                            removeSetAndDeal gs t1 t2 t3
                    in
                    ( newPlayer |> clearSelection |> incrPoints, newGameState )

        deselect : TableCard -> Player -> Player
        deselect t player =
            let
                selectedCards =
                    getSelectedCards player

                newSelectedCards =
                    removeFromSelection t selectedCards

                newPlayer =
                    putSelectedCards newSelectedCards player
            in
            newPlayer

        clearSelection : Player -> Player
        clearSelection p =
            let
                clear record =
                    { record | selectedCards = SZero, selectionStarted = Nothing }
            in
            case p of
                X record ->
                    X (clear record)

                Y record ->
                    Y (clear record)

        dealThree : Deck -> ( ( Card, Card, Card ), Maybe Deck )
        dealThree { nextDraw, rest } =
            case List.take 3 rest of
                [] ->
                    ( nextDraw, Nothing )

                [ _ ] ->
                    ( nextDraw, Nothing )

                [ _, _ ] ->
                    ( nextDraw, Nothing )

                c1 :: c2 :: c3 :: _ ->
                    ( nextDraw, Just { nextDraw = ( c1, c2, c3 ), rest = List.drop 3 rest } )

        removeSetAndDeal : GameState -> TableCard -> TableCard -> TableCard -> GameState
        removeSetAndDeal gs t1 t2 t3 =
            let
                index1 =
                    getTableIndex t1

                index2 =
                    getTableIndex t2

                index3 =
                    getTableIndex t3
            in
            case gs of
                WithEmptyDeck currentTime sparseTable ->
                    WithEmptyDeck
                        currentTime
                        (Dict.union
                            (Dict.fromList [ ( index1, Nothing ), ( index2, Nothing ), ( index3, Nothing ) ])
                            sparseTable
                        )

                WithDeck currentTime deck tbl ->
                    let
                        dealtTable : Card -> Card -> Card -> Table
                        dealtTable c1 c2 c3 =
                            Dict.union
                                (Dict.fromList [ ( index1, c1 ), ( index2, c2 ), ( index3, c3 ) ])
                                tbl
                    in
                    case dealThree deck of
                        ( ( c1, c2, c3 ), Nothing ) ->
                            WithEmptyDeck currentTime (dealtTable c1 c2 c3 |> Dict.map (\_ c -> Just c))

                        ( ( c1, c2, c3 ), Just newDeck ) ->
                            WithDeck currentTime newDeck (dealtTable c1 c2 c3)

        handleSelectionOvertime : GameState -> Player -> ( GameState, Player )
        handleSelectionOvertime gs player =
            let
                selectedCards =
                    getSelectedCards player
            in
            case checkSelectionForSet selectedCards of
                Incomplete ->
                    ( gs, player |> clearSelection |> decrPoints )

                Invalid ->
                    ( gs, player |> clearSelection |> decrPoints )

                Valid t1 t2 t3 ->
                    ( removeSetAndDeal gs t1 t2 t3, player |> clearSelection |> incrPoints )
    in
    case msg of
        StartGame ->
            ( model, generate NewDeck (shuffle cards) )

        EndGame ->
            ( { model | playState = GameOver }, Cmd.none )

        NewDeck cds ->
            let
                tbl =
                    List.take 12 cds
                        |> List.indexedMap (\i cd -> ( i, cd ))
                        |> Dict.fromList

                remainingDeck =
                    List.drop 12 cds
            in
            case remainingDeck of
                [] ->
                    ( model, message EndGame )

                [ _ ] ->
                    ( model, message EndGame )

                [ _, _ ] ->
                    ( model, message EndGame )

                c1 :: c2 :: c3 :: rest ->
                    let
                        newDeck =
                            { nextDraw = ( c1, c2, c3 ), rest = rest }
                    in
                    ( { model | playState = PlayingGame <| WithDeck (Time.millisToPosix 0) newDeck tbl }, Cmd.none )

        Tick newTime ->
            case playState of
                BeforeFirstGame ->
                    ( model, Cmd.none )

                GameOver ->
                    ( model, Cmd.none )

                PlayingGame gameState ->
                    let
                        remainingTime player =
                            remainingSelectionTime player newTime selectionDuration

                        tickedGameState =
                            putCurrentTime newTime gameState

                        tickedModel =
                            model |> updatePlayState (PlayingGame tickedGameState)

                        maybeHandleOvertime remaining player m =
                            if remaining > 0 then
                                ( m, Cmd.none )

                            else
                                let
                                    ( newGameState, newPlayer ) =
                                        handleSelectionOvertime gameState player
                                in
                                ( m |> updatePlayState (PlayingGame newGameState) |> updatePlayer newPlayer, Cmd.none )
                    in
                    case ( remainingTime xPlayer, remainingTime yPlayer ) of
                        ( Nothing, Nothing ) ->
                            ( tickedModel, Cmd.none )

                        ( Just remaining, _ ) ->
                            maybeHandleOvertime remaining xPlayer tickedModel

                        ( _, Just remaining ) ->
                            maybeHandleOvertime remaining yPlayer tickedModel

        StartSelection player ->
            ( model, Time.now |> Task.perform (SetStartTime player) )

        SetStartTime p time ->
            let
                newPlayer =
                    putSelectionStartedTime time p
            in
            ( model |> updatePlayer newPlayer, Cmd.none )

        Select p i ->
            case playState of
                BeforeFirstGame ->
                    ( model, Cmd.none )

                GameOver ->
                    ( model, Cmd.none )

                PlayingGame gameState ->
                    let
                        ( newPlayer, newGameState ) =
                            select i p gameState
                    in
                    ( model |> updatePlayer newPlayer |> updatePlayState (PlayingGame newGameState), Cmd.none )

        Deselect p i ->
            let
                newPlayer =
                    deselect i p
            in
            ( model |> updatePlayer newPlayer, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view ({ selectionDuration, playState, xPlayer, yPlayer } as model) =
    let
        tableCardAttributes : TableCard -> List (Attribute Msg)
        tableCardAttributes t =
            case ( getSelectionStarted xPlayer, getSelectionStarted yPlayer ) of
                ( Nothing, Nothing ) ->
                    [ class "card" ]

                ( Just _, _ ) ->
                    if isSelectedBy t xPlayer then
                        [ class "card", class "set", onClick (Deselect xPlayer t) ]

                    else
                        [ class "card", onClick (Select xPlayer t) ]

                ( _, Just _ ) ->
                    if isSelectedBy t yPlayer then
                        [ class "card", class "set", onClick (Deselect yPlayer t) ]

                    else
                        [ class "card", onClick (Select yPlayer t) ]

        fullTableCardViews table =
            table
                |> Dict.toList
                |> List.map
                    (\( i, cd ) ->
                        div (tableCardAttributes (mkTableCard i cd))
                            [ div [ class "card-box" ] [ img [ src (cardImgPath cd) ] [] ] ]
                    )

        sparseTableCardViews sparseTable =
            sparseTable
                |> Dict.toList
                |> List.map
                    (\( i, card ) ->
                        div [ class "no-card" ] []
                    )

        tableView gameState =
            div
                [ id "table", class "twelve-cards" ]
                (case gameState of
                    WithDeck _ _ table ->
                        fullTableCardViews table

                    WithEmptyDeck _ sparseTable ->
                        sparseTableCardViews sparseTable
                )

        remainingTime currentTime player =
            remainingSelectionTime player currentTime selectionDuration

        remainingTimeChildren currentTime player =
            case remainingTime currentTime player of
                Nothing ->
                    []

                Just remaining ->
                    [ text (String.fromInt remaining) ]

        xRemainingTimeView currentTime =
            div [ id "x-remaining-time" ]
                (remainingTimeChildren currentTime xPlayer)

        yRemainingTimeView currentTime =
            div [ id "y-remaining-time" ]
                (remainingTimeChildren currentTime yPlayer)

        logo =
            img [ id "logo", src "./img/set-logo.png" ] []

        centerPieceView currentTime =
            div [ id "centerpiece" ]
                (case (remainingTime currentTime xPlayer, remainingTime currentTime yPlayer) of
                    (Nothing, Nothing) ->
                        [ logo ]

                    (Just remaining, _) ->
                        [ text (String.fromInt (remaining // 1000)) ]

                    (_, Just remaining) ->
                        [ text (String.fromInt (remaining // 1000)) ]
                )

        dashboardView gameState =
            let
                currentTime =
                    getCurrentTime gameState
            in
            div [ id "dashboard" ]
                [ div
                    [ id "x-player" ]
                    [ text (String.fromInt (getPoints xPlayer)) ]
                , centerPieceView currentTime
                , div
                    [ id "y-player" ]
                    [ text (String.fromInt (getPoints yPlayer)) ]
                ]

        containerAttributes =
            case playState of
                BeforeFirstGame ->
                    [ id "container", class "before-first-game" ]

                GameOver ->
                    [ id "container", class "game-over" ]

                PlayingGame gameState ->
                    let
                        currentTime =
                            getCurrentTime gameState
                    in
                    case ( remainingTime currentTime xPlayer, remainingTime currentTime yPlayer ) of
                        ( Nothing, Nothing ) ->
                            [ id "container", onClick (StartSelection xPlayer) ]

                        ( _, _ ) ->
                            [ id "container" ]

        beforeFirstGameView =
            div [ id "before-first-game-menu" ]
                [ button [ onClick StartGame ] [ text "Play" ] ]

        gameOverView =
            div [ id "game-over-menu" ]
                [ span [] [ text "Game Over" ]

                -- TODO: add final score --
                , button [ onClick StartGame ] [ text "Play again" ]
                ]

        flashView on =
            div [ id "flash-view", class (if on then "on" else "off") ] [ logo ]

        justCalledSet currentTime =
            let delta = 100 in
            case ( remainingTime currentTime xPlayer, remainingTime currentTime yPlayer ) of
                (Nothing, Nothing) ->
                    False

                (Just remaining, _) ->
                    selectionDuration - remaining < delta

                (_, Just remaining) ->
                    selectionDuration - remaining < delta

        containerChildren =
            case playState of
                BeforeFirstGame ->
                    [ beforeFirstGameView ]

                GameOver ->
                    [ gameOverView ]

                PlayingGame gameState ->
                    let currentTime = getCurrentTime gameState in
                    [ dashboardView gameState, tableView gameState, flashView (justCalledSet currentTime) ]

    in
    div containerAttributes containerChildren



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
