import Dict
import Mouse
import Keyboard

type Cell = (Int,Int)
type Board = Dict.Dict Cell Bool

-- default parameters
cellWidth = 16
cellHeight = 16

colCount = 25
rowCount = 25

boardWidth = cellWidth * colCount
boardHeight = cellHeight * rowCount

defaultBoard : Board
defaultBoard = replicate colCount False |> replicate rowCount
               |> zipCoords |> concat |> Dict.fromList

-- input signals
data Action = Flip Cell | Step

speed : Signal Time
speed = every <| 0.3 * second

paused : Signal Bool
paused = foldp (\_ isPaused -> not isPaused) False
               (keepIf id False Keyboard.space)

steps : Signal Action
steps = constant Step |> sampleOn speed |> dropWhen paused Step

flips : Signal Action
flips = dropRepeats
        <| (\(mx, my) -> Flip (mx `div` cellWidth,
                               my `div` cellHeight))
        <~ (keepIf (\(mx, my) -> mx <= boardWidth &&
                                 my <= boardHeight &&
                                 mx >= 0 && my >= 0)
                   (-1, -1)
        <| keepWhen Mouse.isDown (-1, -1)
                    Mouse.position)

actions = merge steps flips

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
flipCell cell board = Dict.insert cell
                                  (not <| Dict.findWithDefault False cell board)
                                  board

stepBoard : Action -> Board -> Board
stepBoard action board =
    case action of
         Step -> stepSim board
         Flip cell -> flipCell cell board

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
boardState = foldp stepBoard defaultBoard actions

displayBoard : Board -> Element
displayBoard board = 
    collage boardWidth boardHeight
            <| map (\((cx, cy), state) ->
                     move (-(boardWidth / 2 - cellWidth / 2) + (toFloat cx) * cellWidth,
                           (boardHeight / 2 - cellHeight / 2) - (toFloat cy) * cellHeight) .
                     filled (if state then blue else grey)
                            <| rect (cellWidth - 1) (cellHeight - 1))
                   <| Dict.toList board

displayGame : Board -> Bool -> Element
displayGame board isPaused =
    let boardDisp = displayBoard board in
    if isPaused
       then layers [ boardDisp
                   , plainText "Paused" ]
       else boardDisp
                   
main : Signal Element
main = displayGame <~ boardState ~ paused
