{- 
Authors: 
120018523
130011035
130002839
-}

module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw

import Debug.Trace

{- Create a function to delete decimal for positive and round down for negative-}
truncX :: Float -> Int
truncX a =
    if (a < 0)
        then truncate (a) - 1
        else truncate (a)

truncY :: Float -> Int
truncY a =
    if (a > 0)
        then truncate (a) + 1
        else truncate (a)

{- Functions that check whenever user clickes the left button of the mouse, if he either places a new piece, saves, loads, undos or wants a hint.-}
handleInput :: Event -> World -> IO World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w@(World b _ _)
    = if ((insideOf (ux, uy) (x, y) 44 44) == True)
        then return w {board = (board w) {pieces = (undo(board w))}}
        else if((insideOf (hx, hy) (x, y) 50 48) == True)
                then return w {board = (board w) {hint = ((getHint (board w) (turn w)))}}
                else if(insideOf (sx, sy) (x, y) 36 36)
                    then do
                        (saveButton (w)) 
                        return w
                        else if(insideOf (lx, ly) (x, y) 37 36)
                            then do
                                loadButton
                            else if (checkRules (board w))
                                then return w
                                else
                                    if (checkDraw (board w))
                                    then return w
                                    else case (checkWon (board w)) of
                                        Nothing -> do 
                                                    let boardSize = size (board w) - 1
                                                    let width = gridDim (board w)
                                                    let x' = 
                                                            if (boardSize `mod` 2 == 0)
                                                                then truncX ((x + fromIntegral(width) + fromIntegral(boardSize * width)) / fromIntegral(width * 2))
                                                                else truncX ((x + fromIntegral(width) + fromIntegral((boardSize - 1) * width)) / fromIntegral(width * 2))
                                                    let y' = 
                                                            if (boardSize `mod` 2 == 0)
                                                                then - truncY ((y - fromIntegral(width) - fromIntegral(boardSize * width)) / fromIntegral(width * 2))
                                                                else - truncY ((y - fromIntegral(width) - fromIntegral((boardSize + 1) * width)) / fromIntegral(width * 2))
                                                    let b = removeMaybe (placePiece (board w) (turn w) (x',y'))
                                                    case b of 
                                                        Left err -> trace (err) return w
                                                        Right x -> trace(show(turn w) ++ " : " ++ show(x',y')) return (World (x) (other (turn w)) (opts w))
                                        Just x -> return w
        where ux = ((fromIntegral(((size (board w) - 1) * (gridDim (board w))) + 50)))
              uy = (fromIntegral(-(2 * (gridDim (board w))) + ((size (board w) - 1) * (gridDim (board w)))))
              sx = ((fromIntegral(((size (board w) - 1) * (gridDim (board w))) + 50)))
              sy = ((fromIntegral)(-(2 * (gridDim (board w))) + ((size (board w) - 1) * (gridDim (board w)))) - 50)
              hx = ((fromIntegral(((size (board w) - 1) * (gridDim (board w))) + 50)))
              hy = ((fromIntegral)(-(2 * (gridDim (board w))) + ((size (board w) - 1) * (gridDim (board w)))) - 150)
              lx = fromIntegral((size (board w) * (gridDim (board w)) + 50))
              ly = fromIntegral((-(2 * (gridDim (board w)))) + (size (board w) * (gridDim (board w)) - 100))

handleInput e w = return w

-- This function checks if a certain click is within a rectangle
insideOf :: Point -> Point -> Float -> Float -> Bool
insideOf (fx, fy) (x, y) w h = within_x && within_y
  where within_x = fx >= x - w / 2 && fx <= x + w / 2
        within_y = fy >= y - h / 2 && fy <= y + h / 2

