import Dict
import open Automaton
import Mouse

type ScreenPos = (Int,Int)

type Cell = (Int,Int)
type Board = Dict.Dict Cell Bool

-- default parameters
cellWidth = 16
cellHeight = 16

colCount = 30
rowCount = 30

boardWidth = cellWidth * colCount
boardHeight = cellHeight * rowCount

defaultBoard : Board
defaultBoard = replicate rowCount False |> replicate colCount
               |> zipCoords |> concat |> Dict.fromList

-- input signals
type RawInput = { dt : Time
                , pos : ScreenPos
                , isDown : Bool }

data ClickState = Clicking | Clicked Cell | Up
type InputState = { timeSinceStep : Time
                  , clickState : ClickState }

data Action = Flip Cell | None
type BoardInput = { step : Bool
                  , action : Action }

delta : Signal Time
delta = fps 30

timeBetweenSteps : Time
timeBetweenSteps = 0.3 * second

stepInput : RawInput -> InputState -> (InputState,BoardInput)
stepInput {dt, pos, isDown} {timeSinceStep, clickState} =
    let step = if clickState /= Up
                  then False
                  else timeSinceStep >= timeBetweenSteps
        (mx, my) = pos
        cell = (mx `div` cellWidth,
                my `div` cellHeight) in
    ({ timeSinceStep = if step then 0
                               else timeSinceStep + dt
     , clickState = case (isDown, clickState) of
                         (False, _) -> Up
                         (True, Up) -> Clicking
                         (True, Clicking) -> Clicked cell
                         (True, Clicked cell') ->
                             if cell' == cell
                                then Clicked cell
                                else Clicking },

     { step = step
     , action = case clickState of
                     Up -> None
                     Clicking -> Flip cell
                     Clicked cell -> None })

inputAutomaton : Automaton RawInput BoardInput
inputAutomaton = hiddenState { timeSinceStep = 0
                             , clickState = Up }
                             stepInput

input : Signal RawInput
input = RawInput <~ delta ~ Mouse.position ~ Mouse.isDown

-- game logic
neighborDiffs : [(Int,Int)]
neighborDiffs = [(-1, -1), (0, -1), (1, -1),
                 (-1, 0),           (1, 0),
                 (-1, 1),  (0, 1),  (1, 1)]

newState : Bool -> Int -> Bool
newState state n =
         if state
            then if | n < 2 -> False
                    | n == 2 || n == 3 -> True
                    | n > 3 -> False

            else if | n == 3 -> True
                    | otherwise -> False

countNeighbors : Board -> Int -> Int -> Int -- Board -> Cell -> Int
countNeighbors board cx cy =
    let neighbors = map (\(dx, dy) ->
                          Dict.findWithDefault False
                                               (cx + dx, cy + dy)
                                               board)
                    neighborDiffs in
    sum <| map (\state -> if state then 1 else 0) neighbors

stepCell : Board -> Cell -> Bool -> Board -> Board
stepCell board cell state board' = let n = uncurry (countNeighbors board) cell in -- hack for issue #318 in 0.10
                                   Dict.insert cell (newState state n) board'

stepSim : Board -> Board
stepSim board = Dict.foldr (stepCell board) Dict.empty board

flipCell : Cell -> Board -> Board
flipCell cell board = Dict.insert cell (not <| Dict.findWithDefault False cell board)
                                  board

stepBoardInput : BoardInput -> Board -> Board
stepBoardInput {step, action} board =
    case action of
         Flip cell -> flipCell cell board
         None -> if step
                    then stepSim board
                    else board

boardAutomaton : Automaton BoardInput Board
boardAutomaton = state defaultBoard stepBoardInput

-- utilities
replicate : Int -> a -> [a]
replicate n x = if n == 0
                   then []
                   else x :: replicate (n - 1) x

zipCoords : [[a]] -> [[(Cell,a)]]
zipCoords rs = let rsWithX = map (\r -> zip [0..length r - 1] r) rs in
                   zipWith (\r y -> zipWith (\(x, t) y -> ((x, y), t)) r
                                            <| replicate (length r) y)
                           rsWithX
                           [0..length rsWithX]

-- UI
boardState : Signal Board
boardState = run (inputAutomaton >>> boardAutomaton)
                 defaultBoard
                 input

displayBoard : Board -> Element
displayBoard board = collage boardWidth boardHeight
                             <| map (\((cx, cy), state) ->
                                      move (-(boardWidth / 2) + (toFloat cx) * cellWidth,
                                            (boardHeight / 2) - (toFloat cy) * cellHeight) .
                                      filled (if state then blue else grey)
                                             <| rect cellWidth cellHeight)
                                    <| Dict.toList board


main : Signal Element
main = displayBoard <~ boardState
