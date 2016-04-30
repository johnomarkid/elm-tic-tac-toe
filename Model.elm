module Model (..) where

import Random exposing (generate, int, initialSeed)


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
