module Main (..) where

import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Random exposing (initialSeed)
import Model exposing (..)
import GameLogic exposing (updateSquare, checkWin, changeTurn, randLocation)


-- Helper


markToString : Mark -> String
markToString mark =
  case mark of
    X ->
      "X"

    O ->
      "O"

    NA ->
      ""



-- Set up mailboxes and Signals


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


model : Signal Model
model =
  -- initialize model with board size and random seed
  Signal.foldp update (initialModel 2 (initialSeed 6773)) actions.signal



--Update


type Action
  = NoOp
  | Reset
  | IncreaseBoard
  | Turn Location


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Reset ->
      initialModel 2 model.randSeed

    IncreaseBoard ->
      initialModel (model.boardSize + 1) model.randSeed

    Turn location ->
      let
        newModel =
          updateSquare location model
            |> checkWin
            |> (\aModel -> changeTurn aModel)
            |> (\bModel ->
                  -- this is a recursive call to update for the cpu move if turn == O
                  -- can't call this if someone has won because causes infinite loop
                  if bModel.status /= Ongoing then
                    changeTurn bModel
                  else
                    (if bModel.turn == O then
                      let
                        ( loc, newModel ) =
                          randLocation bModel
                      in
                        update (Turn loc) newModel
                     else
                      bModel
                    )
               )
      in
        newModel



-- View


squareView : Int -> Int -> Mark -> Element
squareView rowNum colNum mark =
  let
    location =
      ( rowNum, colNum )

    action =
      if mark == NA then
        (Turn location)
      else
        NoOp
  in
    button (Signal.message actions.address action) (markToString mark)


rowView : Int -> Int -> List Mark -> Element
rowView bs rowNum items =
  let
    squares =
      List.map2 (squareView rowNum) [0..bs] items
  in
    flow right squares


view : Signal.Address Action -> Model -> Element
view address model =
  let
    board =
      if model.status == Ongoing then
        flow
          down
          (List.map2 (rowView model.boardSize) [0..model.boardSize] model.board)
      else
        show "Play again!"

    buttons =
      [ button (Signal.message actions.address Reset) "Reset Game"
      , button (Signal.message actions.address IncreaseBoard) "Increase Board Size"
      ]

    statusMessage =
      (case model.status of
        Win ->
          (markToString model.turn) ++ " Wins!"

        Tie ->
          "It's a Tie!"

        Ongoing ->
          "Game is ongoing."
      )
  in
    flow
      down
      [ board, (flow right buttons), (show statusMessage) ]


main : Signal Element
main =
  Signal.map (view actions.address) model
