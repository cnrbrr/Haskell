{- 
Authors: 
120018523
130011035
130002839
-}

module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

{- Functions that receives coordinates and translate them to on screen coordinates using a formula created by us.-}
placeOnCorPiece :: World -> Position -> Picture -> Picture
placeOnCorPiece w (x,y) p = 
	if (boardSize `mod` 2 == 0)
		then
			Translate ((fromIntegral ((2 * x * gridWidth) - (boardSize * gridWidth)))) ((fromIntegral(- (2 * y * gridWidth) + (boardSize * gridWidth))) ) (p)
		else
			Translate ((fromIntegral ((2 * x * gridWidth) - ((boardSize - 1) * gridWidth))) ) ((fromIntegral(- (2 * y * gridWidth) + ((boardSize + 1)* gridWidth)))) (p)
	where 
		gridWidth = gridDim (board w)
		boardSize = size (board w) - 1


piecesCol :: World -> Picture -> Picture -> [(Position, Col)] -> [Picture]
piecesCol _ wh bl [] = []
piecesCol w wh bl ((pos, col):xs) = 
		img : piecesCol w wh bl xs
	where 
		img =
			if (col == Black)
				then
					(Color black $ placeOnCorPiece w pos bl)
				else
					(Color white $ placeOnCorPiece w pos wh)

{- Function for drawing the Undo image -}
drawUndo :: World -> Picture -> Picture
drawUndo w undo = Translate ((fromIntegral((boardSize * gridWidth) + 50))) ((fromIntegral)(-(2 * gridWidth) + (boardSize * gridWidth))) (undo)
	where 
		gridWidth = gridDim (board w)
		boardSize = size (board w) - 1
{- Function for drawing the Save image -}
drawSave :: World -> Picture -> Picture
drawSave w save= Translate ((fromIntegral((boardSize * gridWidth) + 50))) ((fromIntegral)(-(2 * gridWidth) + (boardSize * gridWidth)) - 50) (save)
	where 
		gridWidth = gridDim (board w)
		boardSize = size (board w) - 1
{- Function for drawing the Load image -}
drawLoad :: World -> Picture -> Picture
drawLoad w load = Translate ((fromIntegral((boardSize * gridWidth) + 50))) ((fromIntegral)(-(2 * gridWidth) + (boardSize * gridWidth)) - 100) (load)
	where 
		gridWidth = gridDim (board w)
		boardSize = size (board w) - 1
{- Function for drawing the Hint image -}
drawHint :: World -> Picture -> Picture
drawHint w hint = Translate ((fromIntegral((boardSize * gridWidth) + 50))) ((fromIntegral)(-(2 * gridWidth) + (boardSize * gridWidth)) - 150) (hint)
	where 
		gridWidth = gridDim (board w)
		boardSize = size (board w) - 1
{- Function for drawing the board of the given "world" -}
drawWorld :: World -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> IO Picture
drawWorld w a undo wh b save load h = 
	if(isNothing (hint (board w)))
		then return( Pictures [color (makeColor 0.2 0.7 0.5 1.2) $ a, Pictures grid,
				  Pictures pcs, (drawUndo w undo), (drawSave w save), (drawLoad w load), (drawHint w h)])
		else return (Pictures [color (makeColor 0.2 0.7 0.5 1.2) $ a, Pictures grid,
				  Pictures pcs, (drawUndo w undo), (drawSave w save), (drawLoad w load), (drawHint w h), (placeOnCorPiece w (fromJust(hint(board w))) (color yellow $ circle 10))])

	where
		board' = board w
		listPcs = pieces board'
		size' = size (board')
		gridWidth = gridDim (board w)
		grid =
			if (size' `mod` 2 == 0)
		    	then [Line[((fromIntegral x), -(fromIntegral (gridSize - gridWidth))), ((fromIntegral x), (fromIntegral (gridSize + gridWidth)))] | x <- [-gridSize..gridSize + gridWidth], x `mod` (gridWidth * 2) == 0] ++
			   		 [Line[(-(fromIntegral (gridSize - gridWidth)), (fromIntegral y)), ((fromIntegral (gridSize + gridWidth)), (fromIntegral y))] | y <- [-gridSize..gridSize + gridWidth], y `mod` (gridWidth * 2) == 0]
		   	 	else [Line[((fromIntegral x), -(fromIntegral gridSize)), ((fromIntegral x), (fromIntegral gridSize))] | x <- [-gridSize..gridSize], x `mod` (gridWidth * 2) == 0] ++
			   	 	 [Line[(-(fromIntegral gridSize), (fromIntegral y)), ((fromIntegral gridSize), (fromIntegral y))] | y <- [-gridSize..gridSize], y `mod` (gridWidth * 2) == 0]
			where 
				gridSize = (size' - 1 ) * gridWidth

		pcs = piecesCol w wh b listPcs 