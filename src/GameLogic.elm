module GameLogic exposing (..)

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


scoreUpVertical : Mark -> Board -> List number
scoreUpVertical mark board =
  let
    boardLength =
      ((List.length board) - 1)
  in
    List.map2
      (\row index ->
        if (get (boardLength - index) row) == mark then
          1
        else
          0
      )
      board
      [0..boardLength]


scoreDownVertical : Mark -> Board -> List number
scoreDownVertical mark board =
  List.map2
    (\row index ->
      if (get (0 + index) row) == mark then
        1
      else
        0
    )
    board
    [0..((List.length board) - 1)]


upVerticalLocations : Board -> List Location
upVerticalLocations board =
  let
    boardLength =
      ((List.length board) - 1)
  in
    List.map2
      (\row index ->
        (index, (boardLength - index))
      )
      board
      [0..boardLength]

downVerticalLocations : Board -> List Location
downVerticalLocations board =
  List.map2
    (\_ index ->
      (index, index)
    )
    board
    [0..((List.length board) - 1)]

potentialScore : Location -> Mark -> Board -> Int
potentialScore location mark board =
  -- calculate the score for rows, cols, and verticals
  -- return the max score
  let
    rowScore =
      get (fst location) board
      |> List.map (\v -> if v == mark then 1 else 0)
      |> Debug.log "row: "
      |> List.sum

    colScore =
      invertScoreboard board
      |> get (snd location)
      |> List.map (\v -> if v == mark then 1 else 0)
      |> Debug.log "col: "
      |> List.sum

    -- only look at verticals if our location is on the vertical
    upVerticalScore =
      if List.member location (upVerticalLocations board) then
        scoreUpVertical mark board
        |> Debug.log "up vert: "
        |> List.sum
      else
        0

    downVerticalScore =
      if List.member location (downVerticalLocations board) then
        scoreDownVertical mark board
        |> Debug.log "down vert: "
        |> List.sum
      else
        0

  in
    List.maximum [ rowScore, colScore, upVerticalScore, downVerticalScore]
    |> fromJust

createPotentialScoreboard : Mark -> Board -> List (List (Int, Location))
createPotentialScoreboard mark board =
  (List.map2
    (\row items ->
      List.map2
        (\col item ->
          if item == NA then
            -- calculate potential score for location if location is empty
            (potentialScore (row, col) mark board, (row, col))
          else
            -- location empty. not a possible move.
            (0, (row, col))
        )
        [0..((List.length board) - 1)]
        items
    )
    [0..((List.length board) - 1)]
    board
  )

maxLocation : List (List (Int, Location)) -> Location
maxLocation board =
  -- have board of potential scores and locations like:
  -- [(2, (0, 2)), (1, (1, 2)), ..]
  -- find the max score and that is the location we want to place our cpu move
  let
    loc =
      List.concat board
      |> List.sortBy fst
      |> List.reverse
      |> get 0
      |> snd
  in
    loc


omarmax : Model -> (Location, Model)
omarmax model =
  let
  -- find available locations on board
  -- calculate potential X score for each location
  -- return the location that X has gained the most points in
    turnLocation =
      createPotentialScoreboard X model.board
      |> Debug.log "scoreboard: "
      |> maxLocation

  in
    (turnLocation, model)




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
