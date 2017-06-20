module REPL where

import Expr
import Parsing

data State = State { vars :: [(Name, Float)],
                     numCalcs :: Int,
                     history :: [Command] }

initState :: State
initState = State [] 0 []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
-- for simplicity, using dropVar we try and remove the value anyway
updateVars :: Name -> Float -> [(Name, Float)] -> [(Name, Float)]
updateVars name value vars = (name, value) : (dropVar name vars)

addVars :: State -> [(Name, Float)] -> State
addVars state v = state { vars = v}

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Float)] -> [(Name, Float)]
dropVar name vars = filter (\x -> fst x /= name) vars
-- uses filter to remove any variables that match the name 

addHistory :: State -> Command -> State
addHistory st command = st {history = command: history st, numCalcs = (numCalcs st + 1)}

retrieveHistory :: State -> Int -> Command
retrieveHistory st loc = (history st) !! loc

process :: State -> Command -> IO ()
process st (Set var e) 
     = do let st' = addHistory st (Set var e)
          case (eval (vars st) e) of
            Just x -> do putStrLn "OK"
                         repl (addVars st' (updateVars var x (vars st')))
            _ -> do putStrLn "an error during set"
                    repl st'
process st (Eval e) 
     = do let st' = addHistory st (Eval e)
          case (eval (vars st) e) of
            Just x -> do putStrLn (show x)
            --Here we call repl again, updating the "it" variable to contain the latest value
                         repl (addVars st' (updateVars "it" x (vars st')))
            _ -> do putStrLn ("an error during eval")
          -- Print the result of evaluation
                    repl st'
process st (Hist e)
--This is the custom process for handling the fetch history command
-- The conditional is to handle values longer than the history
    = if (e > length (history st))
          then process st (retrieveHistory st (length (history st) - 1))
          else process st (retrieveHistory st e)
          -- recursively calls process with the retrieved command
process _ (Quit)
    = putStrLn "Thanks! Good Bye"

--- Read, Eval, Print Loop
--- This reads and parses the input using the pCommand parser, and calls
--- 'process' to process the command.
--- 'process' will call 'repl' when done, so the system loops.
repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- getLine
             case parse pCommand inp of
                    [(cmd, "")] -> -- Must parse entire input
                            process st cmd
                    _ -> do putStrLn "Parse error"
                            repl st