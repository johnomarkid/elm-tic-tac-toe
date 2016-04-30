module GameLogic (..) where

import Random exposing (generate, int, initialSeed)
import Model exposing (..)


-- Helpers


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
