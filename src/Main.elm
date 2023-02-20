-- This module defines a simple implementation of a Tic Tac Toe game.
-- It provides the user with a 3x3, 4x4, 5x5 board that can be clicked to add X or O symbols.
-- It keeps track of the current player and the game state, and displays a message to the user when the game ends.


module Main exposing (main)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Modal exposing (modal)
import Stylesheet exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view >> toUnstyled, update = update }


type alias Model =
    { board : Board -- The current board state, mapping the positions to players (X or O).
    , currentPlayer : Player -- The current player (X or O).
    , gameState : GameState -- The current game state (Started or Finished).
    , boardSize : Int -- The size of the board (3, 4, or 5).
    , toggleModal : Bool -- Whether to show the initial play modal.
    }



-- The `Player` type defines the two possible players (X or O).


type Player
    = X
    | O



-- The `GameState` type defines the two possible game states (Started or Finished).


type GameState
    = Started
    | Finished (Maybe Player)



-- The `Board` type is an alias for a dictionary mapping a position (x,y) to a player (X or O).


type alias Board =
    Dict ( Int, Int ) Player



-- The initial model, when the game starts.


initialModel : Model
initialModel =
    { board = Dict.empty -- An empty board.
    , currentPlayer = X -- Player X starts.
    , gameState = Started -- The game is just starting.
    , boardSize = 3 -- A 3x3 board.
    , toggleModal = True -- Show the initial play modal.
    }


type Msg
    = SquareClick ( Int, Int ) -- A square on the board was clicked.
    | AddCell -- Increase the board size by 1.
    | DecCell -- Decrease the board size by 1.
    | PlayAgain -- Start a new game.
    | ToggleModal -- Hide the modal to change the board size.



-- This function change the player after each turn


changePlayer : Player -> Player
changePlayer player =
    case player of
        X ->
            O

        O ->
            X


update : Msg -> Model -> Model
update msg model =
    case msg of
        SquareClick ( x, y ) ->
            -- If the game has ended, do nothing.
            if model.gameState /= Started then
                model

            else
                -- Insert the new move into the board.
                let
                    newBoard =
                        Dict.insert ( x, y ) model.currentPlayer model.board
                in
                -- If the move was valid, switch players and check for a winner or a draw.
                if isEmpty model.board ( x, y ) then
                    { model
                        | currentPlayer = changePlayer model.currentPlayer
                        , board = newBoard
                        , gameState =
                            if Dict.size model.board < (model.boardSize * model.boardSize - 1) && checkWinner newBoard model == Nothing then
                                Started

                            else
                                Finished (checkWinner newBoard model)
                    }

                else
                    -- If the move was invalid, do nothing.
                    model

        PlayAgain ->
            -- Reset the game to the initial state.
            initialModel

        AddCell ->
            -- Increase the board size by 1.
            let
                newBoardSize =
                    if model.boardSize > 4 then
                        -- I have limited here to 5 squares at most, but this can work for any number
                        5

                    else
                        model.boardSize + 1
            in
            { model
                | boardSize = newBoardSize
                , board = Dict.empty
                , currentPlayer = X
                , gameState = Started
            }

        DecCell ->
            -- Decrease the board size by 1.
            let
                newBoardSize =
                    if model.boardSize < 4 then
                        3

                    else
                        model.boardSize - 1
            in
            { model
                | boardSize = newBoardSize
                , board = Dict.empty
                , currentPlayer = X
                , gameState = Started
            }

        ToggleModal ->
            -- Hide the modal to change the board size.
            { model | toggleModal = False }



-- Name of Player


writePlayer : Player -> String
writePlayer player =
    case player of
        X ->
            "Player X"

        O ->
            "Player O"



-- Showing the symbol for each player


showPlayer : Player -> String
showPlayer player =
    case player of
        X ->
            "X"

        O ->
            "O"



-- Status of the game. Who's turn is it, whether a player has won, or there was a draw.


viewStatus : GameState -> Player -> String
viewStatus gameState currentPlayer =
    case gameState of
        Finished winner ->
            case winner of
                Just player ->
                    writePlayer player ++ " Won!"

                Nothing ->
                    "Draw"

        Started ->
            "Now playing: " ++ writePlayer currentPlayer



-- Board visualisation, while saving the position of each player (X, Y)


viewBoard : Int -> Board -> List (Html Msg)
viewBoard boardSize board =
    List.range 0 (boardSize - 1)
        |> List.map
            (\x ->
                div [ css [ rowStyle ] ]
                    (List.range 0 (boardSize - 1)
                        |> List.map
                            (\y ->
                                div [ onClick <| SquareClick ( x, y ), css [ squareStyle ] ]
                                    [ case Dict.get ( x, y ) board of
                                        Just player ->
                                            text <| showPlayer player

                                        Nothing ->
                                            text <| ""
                                    ]
                            )
                    )
            )


view : Model -> Html Msg
view model =
    div [ css [ mainStyle ] ]
        [ if model.toggleModal then
            modal model.boardSize ToggleModal DecCell AddCell

          else
            div [] []
        , div [ css [ titleStyle ] ] [ text <| viewStatus model.gameState model.currentPlayer ]
        , div [] <| viewBoard model.boardSize model.board
        , case model.gameState of
            Started ->
                div [] []

            Finished _ ->
                button [ css [ playAgainButtonStyle ], onClick PlayAgain ] [ text "Play Again" ]
        ]



-- Function to get a row from the board


row : Model -> b -> List ( Int, b )
row model y =
    List.range 0 (model.boardSize - 1)
        |> List.map (\x -> ( x, y ))



-- Function to get a column from the board


collumn : Model -> b -> List ( b, Int )
collumn model x =
    List.range 0 (model.boardSize - 1)
        |> List.map (\y -> ( x, y ))



-- Function to get the diagonal from the board (top left to bottom right)


diagonal1 : Model -> List ( Int, Int )
diagonal1 model =
    List.range 0 (model.boardSize - 1)
        |> List.map (\i -> ( i, i ))



-- Function to get the diagonal from the board (top right to bottom left)


diagonal2 : Model -> List ( Int, Int )
diagonal2 model =
    List.range 0 (model.boardSize - 1)
        |> List.map (\i -> ( i, model.boardSize - 1 - i ))



-- Function to get all the winning positions


winPositions : Model -> List (List ( Int, Int ))
winPositions model =
    (List.range 0 (model.boardSize - 1)
        |> List.map (\y -> row model y)
    )
        ++ (List.range 0 (model.boardSize - 1)
                |> List.map (\x -> collumn model x)
           )
        ++ [ diagonal1 model, diagonal2 model ]



-- Function to check if all elements in a list are equal


allEqual : Board -> List ( Int, Int ) -> Maybe Player
allEqual board l =
    case l of
        [] ->
            Nothing

        h :: t ->
            if List.all (\x -> Dict.get h board == Dict.get x board) t then
                Dict.get h board

            else
                Nothing



-- Function to check if a square in the board is empty (i.e. contains 0)


isEmpty : Board -> ( Int, Int ) -> Bool
isEmpty board ( x, y ) =
    case Dict.get ( x, y ) board of
        Just _ ->
            False

        Nothing ->
            True



-- Function to check if a player has won the game


checkWinner : Board -> Model -> Maybe Player
checkWinner board model =
    Maybe.withDefault Nothing
        (List.head <|
            List.filter
                (\x ->
                    case x of
                        Just _ ->
                            True

                        _ ->
                            False
                )
                (List.map (allEqual board) (winPositions model))
        )
