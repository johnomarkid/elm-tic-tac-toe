module Main (..) where

import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Random exposing (generate, int, initialSeed)


-- Model


type alias Model =
  { board : Board
  , turn : Mark
  , status : Status
  , boardSize : Int
  , randSeed : Random.Seed
  }


initialModel : Int -> Random.Seed -> Model
initialModel bs rs =
  { board = initialBoard bs
  , turn = X
  , status = Ongoing
  , boardSize = bs
  , randSeed = rs
  }


type alias Board =
  List (List Mark)


initialBoard : Int -> Board
initialBoard numRows =
  let
    returnNum _ =
      NA
  in
    List.map (\i -> List.map returnNum [0..numRows]) [0..numRows]


type Status
  = Ongoing
  | Tie
  | Win


type Mark
  = X
  | O
  | NA


type alias Location =
  ( Int, Int )



-- Set up mailboxes and Signals


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


model : Signal Model
model =
  -- initialize model with board size and random seed
  Signal.foldp update (initialModel 2 (initialSeed 6773)) actions.signal



-- Helpers


markToString : Mark -> String
markToString mark =
  case mark of
    X ->
      "X"

    O ->
      "O"

    NA ->
      ""


createScoreboard : Mark -> Board -> List (List Int)
createScoreboard mark board =
  (List.map2
    (\row items ->
      List.map2
        (\col item ->
          if item == mark then
            1
          else
            0
        )
        [0..((List.length board) - 1)]
        items
    )
    [0..((List.length board) - 1)]
    board
  )


listToStatus : List Int -> Status
listToStatus items =
  if (List.sum items) == (List.length items) then
    Win
  else
    Ongoing


get : Int -> List a -> a
get n xs =
  List.head (List.drop n xs)
    |> fromJust


fromJust : Maybe a -> a
fromJust x =
  case x of
    Just y ->
      y

    Nothing ->
      Debug.crash "error: fromJust Nothing"


invertScoreboard : List (List a) -> List (List a)
invertScoreboard board =
  List.map (\i -> List.map (\row -> get i row) board) [0..((List.length board) - 1)]


invertUpVertical : List (List number) -> List number
invertUpVertical board =
  let
    boardLength =
      ((List.length board) - 1)
  in
    List.map2
      (\row index ->
        if (get (boardLength - index) row) == 1 then
          1
        else
          0
      )
      board
      [0..boardLength]


invertDownVertical : List (List number) -> List number
invertDownVertical board =
  List.map2
    (\row index ->
      if (get (0 + index) row) == 1 then
        1
      else
        0
    )
    board
    [0..((List.length board) - 1)]


randLocation : Model -> ( Location, Model )
randLocation model =
  let
    randRange =
      int 0 ((List.length model.board) - 1)

    rowResult =
      generate randRange model.randSeed

    colResult =
      generate randRange (snd rowResult)

    -- use new seed from rowResult
    location =
      ( (fst rowResult), (fst colResult) )

    newSeed =
      snd colResult

    newModel =
      { model | randSeed = newSeed }

    markAtLocation =
      get (fst location) model.board
        |> get (snd location)
  in
    if markAtLocation == NA then
      ( location, newModel )
    else
      randLocation newModel



-- Core Gameplay


checkWin : Model -> Model
checkWin model =
  let
    -- if sum of row = 3 for items that match mark
    scoreBoard =
      createScoreboard model.turn model.board

    checkRow =
      scoreBoard
        |> List.map listToStatus

    checkCol =
      scoreBoard
        |> invertScoreboard
        |> List.map listToStatus

    checkUpVertical =
      scoreBoard
        |> invertUpVertical
        |> listToStatus

    checkDownVertical =
      scoreBoard
        |> invertDownVertical
        |> listToStatus

    status =
      List.append checkRow checkCol
        |> (\v -> checkUpVertical :: v)
        |> (\v -> checkDownVertical :: v)
        |> List.member Win
        |> (\v ->
              (if v == True then
                Win
               else
                Ongoing
              )
           )

    -- tie if moves = 0 and no win above
    remainingMoves =
      createScoreboard NA model.board
        |> List.map List.sum
        |> List.sum

    newStatus =
      case remainingMoves == 0 of
        True ->
          if status == Ongoing then
            Tie
          else
            status

        False ->
          status
  in
    { model | status = newStatus }


updateSquare : Location -> Model -> Model
updateSquare location model =
  let
    newBoard =
      (List.map2
        (\x row ->
          List.map2
            (\y item ->
              if ( x, y ) == location then
                model.turn
              else
                item
            )
            [0..model.boardSize]
            row
        )
        [0..model.boardSize]
        model.board
      )
  in
    { model | board = newBoard }


changeTurn : Model -> Model
changeTurn model =
  let
    newTurn =
      case model.turn of
        X ->
          O

        O ->
          X

        NA ->
          NA
  in
    { model | turn = newTurn }



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
