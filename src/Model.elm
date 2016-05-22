module Model exposing (..)

type alias Model =
  { board : Board
  , turn : Mark
  , status : Status
  , boardSize : Int
  }


initialModel : Int -> Model
initialModel bs =
  { board = initialBoard bs
  , turn = X
  , status = Ongoing
  , boardSize = bs
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
