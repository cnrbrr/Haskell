{- 
Authors: 
120018523
130011035
130002839
-}
module Main where

import Graphics.Gloss

import Board
import Draw
import Input
import AI
import System.Environment   
import Data.List
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
	{- Loading the images used in displaying the board-}
	a <- loadBMP "newpicture.bmp"
	w <- loadBMP "whitePiece.bmp"
	b <- loadBMP "blackPiece.bmp"
	s <- loadBMP "save.bmp"
	l <- loadBMP "load.bmp"
	h <- loadBMP "hint.bmp"
	undo <- loadBMP "undo.bmp"
	{- getting the arguments from the command line that are gonna be used as the game's options-}
	args <- getArgs
	{- Checking if the given arguments are valid-}
	if ((args !! 0) == "Player" || (args !! 0) == "AI") then 
		if ((args !! 1) == "Player" || (args !! 1) == "AI") then 
			if ((args !! 4) == "Neutral" || (args !! 4) == "Aggresive" || (args !! 4) == "Defensive") then
				if ((args !! 5) == "Neutral" || (args !! 5 == "Aggresive" || (args !! 5) == "Defensive")) then
					if ((args !! 6 == "1") || (args !! 6) == "2") then
						if ((args !! 6) == "3" && ((args !! 2) /= "6")) then
							putStrLn "The game at this AI setting needs to be put to a smaller stage, please set the board size to six"
						else
						playIO(InWindow "Gomoku" (800, 600) (20, 20)) black 10
						(initWorld args)-- in Board.hs
						(\x -> drawWorld x a undo w b s l h) -- in Draw.hs
						handleInput -- in Input.hs
						(updateWorld) -- in AI.hs
					else putStrLn "Easy must be set to 1, Medium to 2 and Hard to 3."
				else putStrLn "There was an error, as a type of AI which does not exist was entered (AI 2)"
			else putStrLn "There was an error, as a type of AI which does not exist was entered (AI 1)"
		else putStrLn "There was an error, as an incorrect white player was entered"
	else putStrLn "There was an error, as an incorrect black player was entered"