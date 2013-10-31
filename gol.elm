-- type Board = Dict (Int,Int) Bool

neighborDiffs = [(0-1, 0-1), (0, 0-1), (1, 0-1),
                 (0-1, 0),             (1, 0),
                 (0-1, 1),   (0, 1),   (1, 1)]

-- countNeighbors :: Board -> (Int,Int) -> Int
countNeighbors board (cx, cy) = let neighbors = map (\(dx, dy) ->
                                                      Dict.findWithDefault False
                                                                           (cx + dx, cy + dy)
                                                                           board)
                                                    neighborDiffs in
                                sum $ map (\state -> if state then 1 else 0) neighbors

-- newState :: Bool -> Int -> Bool
newState state n =
         if state
            then if | n < 2 -> False
                    | n == 2 || n == 3 -> True
                    | n > 3 -> False

            else if | n == 3 -> True
                    | otherwise -> False

-- stepCell :: Board -> (Int,Int) -> Bool -> Board -> Board
stepCell board pos state board' = let n = countNeighbors board pos in
                                  Dict.insert pos (newState state n) board'

-- stepSim :: a -> Board -> Board
stepSim _ board = Dict.foldr (stepCell board) Dict.empty board

-- displaySim :: Board -> Element
displaySim board = collage 256 256
                           $ map (\((cx, cy), state) ->
                                   move (cx * 16) (cy * 16) .
                                   filled (if state then blue else grey)
                                          $ rect 16 16 (8, 8))
                                 $ Dict.toList board

-- stepClick :: (Int,Int) -> Board -> Board
stepClick (mx, my) board = Dict.insert (mx `div` 16, my `div` 16) board

-- replicate :: Int -> a -> [a]
replicate n x = if n == 0
                   then []
                   else x :: replicate (n - 1) x

-- zipCoords :: [[Tile]] -> [[((Int,Int),Tile)]]
zipCoords rs = let rsWithX = map (\r -> zip [0..length r - 1] r) rs in
                   zipWith (\r y -> zipWith (\(x, t) y -> ((x, y), t)) r
                                            $ replicate (length r) y)
                           rsWithX
                           [0..length rsWithX]

-- initialBoard :: [((Int,Int),Bool)]
initialBoard = concat $ zipCoords [[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,True ,True ,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,True ,True ,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,True ,True ,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,True ,True ,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
                                   [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]]

-- main :: Signal Element
main = displaySim <~ foldp stepSim (Dict.fromList initialBoard) (every second)
