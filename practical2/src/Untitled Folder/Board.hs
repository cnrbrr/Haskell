{- 
Authors: 
120018523
130011035
130002839
-}

module Board where
import Debug.Trace
import System.Environment
import System.IO

{- Data type representing the colour of the pieces.-}
data Col = Black 
         | White
  deriving (Read, Eq, Show)

{- Type representing a tuple of two ints, the coordinates/position of the pieces.-}
type Position = (Int, Int)

{- Data type representing all the information necessary for the board.-}
data Board = Board { size :: Int,
                     gridDim :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)],
                     zones :: [[Position]],
                     hint :: Maybe Position
                   }
  deriving (Read, Show)

{- Data type representing the settings of the game.-}
data Options = Options { black' :: String,
                         white' :: String,
                         boardSize :: Int,
                         aim :: Int,
                         blackAI :: String,
                         whiteAI :: String,
                         aiLevel :: Int
                       }
  deriving (Show, Eq, Read)

{-Data type representing the state of the game.-}
data World = World { board :: Board,
                     turn :: Col, 
                     opts :: Options}
  deriving (Read, Show)

{-Data type representing all the 8 directions.-}
data Dir = N | NW | W | WS | S | SE | E | EN

other :: Col -> Col
other Black = White
other White = Black

{- Initialising optins from the given arguments-}
initOptions :: [String] -> Options
initOptions args = do
  Options (args !! 0) (args !! 1) ((read (args !! 2)) :: Int) ((read (args !! 3)) :: Int) (args !! 4) (args !! 5) ((read (args !! 6)) :: Int)

{- Initialising the board -}
initBoard :: Options -> Board
initBoard (Options _ _ s t _ _ _) = Board s 15 t [] (createZones s) Nothing

{- Returning a direction opposite to the given-}
inverse :: Dir -> Dir
inverse N = S
inverse NW = SE
inverse W = E
inverse WS = EN
inverse S = N
inverse SE = NW
inverse E = W
inverse EN = WS

{- Initialising the state of the game.-}
initWorld :: [String] -> World
initWorld op = World (initBoard options) Black options
  where options = (initOptions op)

undo :: Board -> [(Position, Col)]
undo (Board _ _ _ [] _ _) =[]
undo b = (tail(tail (pieces b)))

{- Functions for saving and loading the game.-}
saveButton :: World -> IO ()
saveButton w =
  writeFile "data.txt" (show(w))


loadButton :: IO World
loadButton = do
  s <- readFile "data.txt"
  return (read s :: World)

{- Checking that a position is within the board's boundaries-}
withinBoard :: Board -> Position -> Bool
withinBoard board (x,y) = (x >=0 && x < (size board)) && (y >=0 && y < (size board))

{- Getting the colour of a piece at a specific location. If there is no piece, "Nothing" is returned.-}
getCol :: Board -> Position -> Maybe Col
getCol board pos = lookup pos (pieces board)

{- Checking if at a position there is a specific coloured piece-}
isCol:: Board -> Col -> Position -> Bool
isCol board col pos = (getCol board pos == Just col)

{- Checking that a piece can be placed at a specific position-}
canPlacePiece :: Board -> Position  -> Bool
canPlacePiece board pos = (withinBoard board pos) && ((getCol board pos) == Nothing)

{- Trying to place a piece. If it is not possible "Nothing" is returned, else the new Board.-}
placePiece :: Board -> Col -> Position -> Maybe Board
placePiece board col pos
  | canPlacePiece board pos = Just $ Board(size board) (gridDim board) (target board) ((pos, col) : pieces board) (zones board)(Nothing)
  | otherwise = Nothing

removeMaybe :: Maybe Board -> Either String Board
removeMaybe (Just b) = Right $ b
removeMaybe Nothing = Left $ fail ("Couldn't place the Piece")

{- Making a given move if possible else "Nothing"-}
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col pos = placePiece board col pos

{-Given a direction and a position return the new position if moved by n in that direction-}
move :: Dir -> Int -> Position -> Position
move _ 0 p = p
move dir n (x,y) = move dir (n-1) $ case dir of
                                      N   -> (x, y-1)
                                      NW  -> (x-1,y-1)
                                      W   -> (x-1,y)
                                      WS  -> (x-1,y+1)
                                      S   -> (x, y+1)
                                      SE  -> (x+1,y+1)
                                      E   -> (x+1,y)
                                      EN  -> (x+1,y-1)


checkDraw :: Board -> Bool
checkDraw b = 
  if ((length list) == (s * s))
    then True
    else False
  where list = (pieces b)
        s = (size b)

checkIfWon :: Board -> Bool
checkIfWon b = 
  case (checkWon' b (pieces b)) of
    Left err -> False
    Right x -> True
 
checkWon :: Board -> Maybe Col
checkWon b = 
  case (checkWon' b (pieces b)) of
    Left err -> Nothing
    Right x -> Just x

checkRules :: Board -> Bool
checkRules b
  | ((length (pieces b)) > 8) && ((evaluate "Neutral" b Black) <= -1500000)   = True
  | otherwise                                                                 = False


checkWon' :: Board -> [(Position, Col)] -> Either String Col
checkWon' b [] = Left $ ("No one won")
checkWon' b ((pos,col) :xs)
  | not (isCol b col (move (inverse N) 1 pos)) && (check b N pos col 1) = Right $ col
  | not (isCol b col (move (inverse NW) 1 pos)) && (check b NW pos col 1) = Right $ col
  | not (isCol b col (move (inverse W) 1 pos)) && (check b W pos col 1) = Right $ col
  | not (isCol b col (move (inverse WS) 1 pos)) && (check b WS pos col 1) = Right $ col
  | not (isCol b col (move (inverse S) 1 pos)) && (check b S pos col 1) = Right $ col
  | not (isCol b col (move (inverse SE) 1 pos)) && (check b SE pos col 1) = Right $ col
  | not (isCol b col (move (inverse E) 1 pos)) && (check b E pos col 1) = Right $ col
  | not (isCol b col (move (inverse EN) 1 pos)) && (check b EN pos col 1) = Right $ col
  | otherwise = checkWon' b xs

check :: Board -> Dir -> Position -> Col -> Int -> Bool
check b d p c i
  | (i == (target b))  = 
    if not (isCol b c (move d i p))
      then True
      else False
  | isCol b c (move d i p) = (check b d p c (i+1))
  | otherwise = False

generateMoves :: Board -> [Position]
generateMoves b = generateMoves' b (pieces b)

generateMoves' :: Board -> [(Position, Col)] -> [Position]
generateMoves' _ [] = []
generateMoves' b ((pos,col):xs) = genList
  where genList = (createRectangle b pos) ++ generateMoves' b xs


createRectangle :: Board -> Position -> [Position]
createRectangle b (x,y) = coor
  where coor' = [(x',y') | x' <- [x-1..x+1], y' <-[y-1.. y+1]]
        coor = filter (\pos -> (canPlacePiece b pos)) coor'

inWhichZone :: Board -> Position  -> Int
inWhichZone b pos = (searchZones pos (zones b) 1)

searchZones :: Position -> [[Position]] -> Int -> Int
searchZones pos [] _ =  0
searchZones pos (x:xs) i=
  if (elem pos x)
    then i 
    else searchZones pos xs (i+1)
  

createZones :: Int -> [[Position]]
createZones s =
  zones
  where 
    s' = fromIntegral s
    n = 
      if (s `mod` 2 == 0)
        then [1..truncate(s' / 2)]
        else [1..truncate((s' + 1) / 2)]
    zones = 
      if (s `mod` 2 == 0)
        then mapSpecial createZoneEven s n
        else mapSpecial createZoneOdd s n

mapSpecial :: (Int -> Int -> [Position]) -> Int -> [Int] -> [[Position]]
mapSpecial fun s [] = []
mapSpecial fun s (x:xs) =
  (fun s x) : mapSpecial fun s xs

createZoneOdd :: Int -> Int -> [Position]
createZoneOdd s z = zone 
  where 
    s' = fromIntegral s
    z' = fromIntegral z
    n1 = truncate ((s' - 1) / 2 - (z' - 1))
    n2 = truncate ((s' - 1) / 2 + (z' - 1))
    zone =
      if (n1 /= n2)
        then 
          [(x,y) | x <- [n1..n2], y <- [n1]] ++
          [(x,y) | x <- [n1..n2], y <- [n2]] ++
          [(x,y) | y <- [(n1+1)..(n2-1)], x <- [n1]] ++
          [(x,y) | y <- [(n1+1)..(n2-1)], x <- [n2]]
        else [(n1,n1)]

createZoneEven :: Int -> Int -> [Position]
createZoneEven s z = zone 
  where 
    s' = fromIntegral s
    z' = fromIntegral z
    n1 = truncate ((s' - 1) / 2 - (z' - (1/2)))
    n2 = truncate ((s' - 1) / 2 + (z' - (1/2)))
    zone =
      if (z /= 1)
        then 
          [(x,y) | x <- [n1..n2], y <- [n1]] ++
          [(x,y) | x <- [n1..n2], y <- [n2]] ++
          [(x,y) | y <- [(n1+1)..(n2-1)], x <- [n1]] ++
          [(x,y) | y <- [(n1+1)..(n2-1)], x <- [n2]]
        else
          [(x,y) | x <- [n1..n2], y <- [n1]] ++
          [(x,y) | x <- [n1..n2], y <- [n2]]

evaluateDir :: Board -> Position -> Col -> Dir -> Bool -> Float
evaluateDir b p c d f
    | not (withinBoard b p)         = 0
    | isCol b (other c) p           = 0
    | getCol b p == Nothing         = 0.1
    | otherwise                     = let r = (evaluateDir b p' c d False)
                                      in if (f)
                                            then (r + 1) + 0.1
                                            else (r + 1)
    where p' = move d 1 p

{- I had a problem with passing 0.1 from one function to another as sometimes it would become 0.999999 and hence i had to check for both 0.1 and 0.999999-}
evaluateLine :: Board -> Position -> Col -> Dir -> Bool -> [Int] -> [Int]
evaluateLine b p c d openBehind rows
  | (line == t || line == (t + (1*spaces)) || line == (t + (2*spaces))) || (line == (t + (1*0.1)) || line == (t + (2*0.1))) = [(rows !! 0)+1] ++ drop 1 rows --5 in a row
  | (line == (t - 1 + ( 2 * spaces))) || (line == (t - 1 + ( 2 * 0.1)))                                                     = take 1 rows ++ [(rows !! 1) + 1] ++ drop 2 rows --4 in a row open-ended
  | (line == (t - 1 + ( 1 * spaces))) || (line == (t - 1 + ( 1 * 0.1)))                                                     = take 2 rows ++ [(rows !! 2) + 1] ++ drop 3 rows --4 in a row one open-ended
  | (line == (t - 2 + ( 2 * spaces))) || (line == (t - 2 + ( 2 * 0.1)))                                                     = take 3 rows ++ [(rows !! 3) + 1] ++ drop 4 rows --3 in a row open-ended
  | (line == (t - 2 + ( 1 * spaces))) || (line == (t - 2 + ( 1 * 0.1)))                                                     = take 4 rows ++ [(rows !! 4) + 1] ++ drop 5 rows --3 in a row one open ended
  | (line == (t - 3 + ( 2 * spaces))) || (line == (t - 3 + ( 2 * 0.1)))                                                     = take 5 rows ++ [(rows !! 5) + 1] ++ drop 6 rows --2 in a row open-ended
  | (line == (t - 3 + ( 1 * spaces))) || (line == (t - 3 + ( 1 * 0.1)))                                                     = take 6 rows ++ [(rows !! 6) + 1] ++ drop 7 rows --2 in a row one open ended
  | otherwise                                                                                                               = rows--trace (show(line) ++ ": on pos " ++ show(p)) 0 --trace("0 /"++ show(line)) 0
  where
    t = fromIntegral (target b)
    line = 
      if (openBehind)
       then evaluateDir b p c d True
       else evaluateDir b p c d False
    spaces = 0.0999999

evaluatePoint :: Board -> Position -> Col -> [Int] -> [Int]
evaluatePoint b p c rows
    = updatedRows
    where
      rows1 = 
        if (getCol b (move (inverse N) 1 p)) == Nothing
          then 
            (evaluateLine b p c N True rows)
          else 
            if (isCol b (other c) (move (inverse N) 1 p)) == True
              then 
                evaluateLine b p c N False rows
              else rows
      rows2 = 
         if (getCol b (move (inverse NW) 1 p)) == Nothing
          then 
            (evaluateLine b p c NW True rows1)
          else 
            if (isCol b (other c) (move (inverse NW) 1 p)) == True
              then 
                evaluateLine b p c NW False rows1
              else rows1
      rows3 = 
        if (getCol b (move (inverse W) 1 p)) == Nothing
          then 
            (evaluateLine b p c W True rows2)
          else 
            if (isCol b (other c) (move (inverse W) 1 p)) == True
              then 
                evaluateLine b p c W False rows2
              else rows2
      rows4 = 
         if (getCol b (move (inverse WS) 1 p)) == Nothing
           then 
             (evaluateLine b p c WS True rows3)
           else 
             if (isCol b (other c) (move (inverse WS) 1 p)) == True
               then 
                 evaluateLine b p c WS False rows3
               else rows3

      updatedRows = take 7 rows4 ++ [(rows !! 7) + zone * (-1 * 5)]
      zone = inWhichZone b p

evaluatePoints :: Board -> [(Position, Col)] -> Col -> [Int] -> [Int]
evaluatePoints _ [] _ xs = xs
evaluatePoints b ((pos,col):xs) evalColour rows =
  if (col == evalColour)
    then
       evaluatePoints b xs evalColour newRows
    else
       evaluatePoints b xs evalColour rows
  where 
    newRows = evaluatePoint b pos col rows

aiAnalysis :: Col -> [Int] -> Int
aiAnalysis col xs
 -- 0 -> 5 in a row, 1 -> 4 in a row open-ended, 2 -> 4 in a row one open-ended,
 -- 3 -> 3 in a row open-ended, 4 --> 3 in a row one open-ended, 5 -> 2 in a row open-ended
 -- 6 -> 2 in a row one open-ended
  | col == Black && (xs !! 3) >= 2              = -2500000
  | col == Black && ((xs !! 1) + (xs !! 2)) > 1 = -2500000
  | (xs !! 0) >= 1                              =  2500000
  | (xs !! 1) >= 1                              =  2500000
  | (xs !! 2) == 1 && (xs !! 3) >= 1            =  1500000
  | (xs !! 2) >= 1                              =  1000000
  | (xs !! 3) >= 2                              =  5000
  | (xs !! 3) >= 1 && (xs !! 4) >=1             =  1000
  | (xs !! 3) == 1                              =  200 + zone
  | (xs !! 5) >= 2                              =  100 + zone
  | (xs !! 4) == 1                              =  50 + zone
  | (xs !! 6) >= 2                              =  10 + zone
  | (xs !! 5) == 1                              =  5 + zone
  | (xs !! 6) == 1                              =  3 + zone
  | otherwise                                   = zone
    where zone = xs !! 7


-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: String -> Board -> Col -> Int
evaluate ai b@(Board _ _ _ pieces _ _) col
  | null pieces = 0
  | ai == "Neutral" = (analysis1 - analysis2)
  | ai == "Aggresive" = analysis1 - round (0.5 * fromIntegral (analysis2))
  | ai == "Defensive" = analysis1 - round (2 * fromIntegral (analysis2))
  where empty = [0,0,0,0,0,0,0,0]  
        myRows = evaluatePoints b pieces col empty
        enemyRows = evaluatePoints b pieces (other col) empty
        analysis1 = aiAnalysis col myRows
        analysis2 = aiAnalysis (other col) enemyRows
