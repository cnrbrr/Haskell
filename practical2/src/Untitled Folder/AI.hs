{- 
Authors: 
120018523
130011035
130002839
-}

module AI where

import Board
import Debug.Trace

data GameTree = GameTree { 
                           game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] 
                         }

updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> IO World
updateWorld t w@(World board turn opts)-- options)
  | checkRules board                                    = trace("Player " ++ show(turn) ++ " has won.") (return w)
  | checkDraw board                                     = trace("It's a Draw.") (return w)
  | checkIfWon board                                    = trace("Player " ++ show(checkWon board) ++ " has won.") (return w)
  | ((turn  == White) && ((white' opts) == "Player"))   = return w
  | ((turn  == Black) && ((black' opts) == "Player"))   = return w
  | otherwise                                           = 
    if (turn == White)
      then (moveAI (whiteAI opts) w)
      else (moveAI (blackAI opts) w)

{- Function that returns a World with a move played by an AI-}
moveAI :: String-> World -> IO World
moveAI ai (World board turn opts) = return (World (newBoard) (other(turn)) (opts))
  where newBoard = Board (size board) (gridDim board) (target board) ([(newMove, turn)] ++ pieces board) (zones board) Nothing
        newMove = getBestMove (ai) (difficulty) (buildTree getValidMoves (board) (turn))
        difficulty = 
          if (aiLevel opts) == 1
            then 0
            else 
              if (aiLevel opts) == 2
                then 1
                else
                  if (aiLevel opts) == 3
                    then 3
                    else 1

buildTree :: (Board -> Col -> [Position])-> Board -> Col -> GameTree
buildTree generate board colour = 
    let moves = generate board colour in GameTree board colour (makeNextStates moves)
  where
    makeNextStates :: [Position] -> [(Position, GameTree)]
    makeNextStates [] = []
    makeNextStates (position:positions)
        = case makeMove board colour position of  
               Nothing -> makeNextStates positions 
               Just newBoard -> (position, buildTree generate newBoard (other colour)) 
                                  : makeNextStates positions

{-If it's the first move, return all the valid positions to play, else return all the positions close to already placed pieces.-}
getValidMoves :: Board -> Col -> [Position]
getValidMoves b col
  | moves == [] = getAllMoves b col
  | otherwise = moves
  where moves = generateMoves b ---trace ("List generated: " ++ show(generateMoves b)) (generateMoves b)

getAllMoves :: Board -> Col -> [Position]
getAllMoves b col =
  [(x,y) | x <- [0..(size b)], y <- [0..(size b)], (isCol b Black (x,y) /= True) && (isCol b White (x,y) /= True)]

{- Minimax Functions-}
getMaximum :: [(t, Int)] -> (t, Int)
getMaximum (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

getPosition :: ((Position, GameTree), Int) -> Position
getPosition ((pos,_),_) = pos

getBestMove :: String -> Int-> GameTree -> Position
getBestMove ai depth gameTree = trace (show (game_turn gameTree) ++ " : " ++ (show choice)) choice
          where getPosition (p,tree) = p
                choice = maxPath ai depth (game_turn gameTree) (next_moves gameTree)

maxPath :: String -> Int -> Col -> [(Position, GameTree)] ->  Position
maxPath ai depth c [(x,y)] = x
maxPath ai depth c xs = pos
  where 
    trees = map (\(x,y) -> y) xs
    list = getMaximum (zip xs $ map (minMax ai depth c False) trees)
    pos = getPosition list

minMax :: String -> Int -> Col -> Bool -> GameTree -> Int
minMax ai _ c _ tree@(GameTree b _ [])                 = evaluate ai b c
minMax ai 0 c _ tree@(GameTree b _ _)                  = evaluate ai b c
minMax ai depth c isMax tree@(GameTree b _ nextMoves)  = 
  if (checkWon b == Just c) then 2500000
    else
      if (checkWon b == Just (other c)) then -2500000
        else
          if isMax then maximum (maxList)
            else minimum (minList)
  where
    trees = map (\(x,y) -> y) nextMoves
    maxList = (map (minMax ai (depth-1) c False) trees)
    minList = (map (minMax ai (depth-1) c True) trees)

{- Get the best next move given a board.-}
getHint :: Board -> Col -> Maybe Position
getHint board color = Just (getBestMove "Neutral" 1 (buildTree getValidMoves (board) (color)))