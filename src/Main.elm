module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Model exposing (..)
import GameLogic exposing (updateSquare, checkWin, changeTurn)


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



--Update


type Msg
  = NoOp
  | Reset
  | IncreaseBoard
  | Turn Location


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    Reset ->
      initialModel 2

    IncreaseBoard ->
      initialModel (model.boardSize + 1)

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
                      -- let
                      --   ( loc, newModel ) =
                      --     randLocation bModel
                      -- in
                        --update (Turn loc) newModel
                        bModel -- temporary
                     else
                      bModel
                    )
               )
      in
        newModel



-- View


squareView : Int -> Int -> Mark -> Html Msg
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
    div [  onClick action
            , style [("width", "50px"), ("height", "50px"), ("background-color", "gray"), ("border", "1px solid black"), ("justify-content", "center"), ("align-items", "center") ,("display", "flex")]]
            [text (markToString mark)]


rowView : Int -> Int -> List Mark -> Html Msg
rowView bs rowNum items =
  let
    squares =
      List.map2 (squareView rowNum) [0..bs] items
  in
    div [style [("display", "flex")]] squares



view : Model -> Html Msg
view model =
  let
    board =
      if model.status == Ongoing then
          div []
            (List.map2 (rowView model.boardSize) [0..model.boardSize] model.board)
      else
        div [] [text "Play again!"]

    buttons =
      div [] [
       button [onClick Reset] [text "Reset Game"]
      , button [onClick IncreaseBoard] [text "Increase Board Size"]]

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
    div [] [board, buttons, div [] [text statusMessage]]

main: Program Never
main =
  Html.beginnerProgram { model = (initialModel 2), view = view, update = update }
