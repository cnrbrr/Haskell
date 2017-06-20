module Expr where

import Parsing
import Debug.Trace

type Name = String

-- At first, 'Expr' contains only addition and values. You will need to 
-- add other operations, and variables
data Expr = Add Expr Expr
          | Val Float
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Abs Float
          | Mod Expr Expr
          | Pow Expr Expr
          | Var Name
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             --the hist command is for when a user enters an "!"
             | Hist Int
             | Quit
  deriving Show

eval :: [(Name, Float)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Float -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' + y')
  _ -> Nothing
eval vars (Sub x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' - y')
  _ -> Nothing
eval vars (Mult x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' * y')
  _ -> Nothing
eval vars (Div x y) = case (eval vars x, eval vars y) of
  --This conditional handles dividing by 0 - the error message isnt helpful
  --but atleast the program doesnt terminate.
  (Just x', Just y') -> if y' /= 0 then Just (x' / y') else Nothing
  _ -> Nothing
--eval vars (Abs x) = Just (abs (x))
--eval vars (Mod x y) = case (eval vars x, eval vars y) of
--  (Just x', Just y') -> Just (`mod'` x' y') 
--  _ -> Nothing
eval vars (Pow x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' ** y')
  _ -> Nothing
eval ((k,v) : xs) (Var x) = if k == x then Just v else eval xs (Var x)
eval [] (Var _) = Nothing

pCommand :: Parser Command
pCommand = do t <- identifier
              char '='
              e <- pExpr
              return (Set t e)
            ||| do e <- pExpr
                   return (Eval e)
                  ||| do char '!'
                         i <- integer
                         return (Hist i)
                        ||| do char ':'
                               char 'q'
                               return (Quit)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Sub t e)
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- float
             return (Val d)
           ||| do v <- identifier
                  if v == "abs"
                    then do d <- float
                            return (Abs d)
                    else return (Var v)
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Mult f t) 
            ||| do char '/'
                   t <- pTerm
                   return (Div f t)
                 ||| do char '%'
                        t <- pTerm
                        return (Mod f t)  
                      ||| do char '^'
                             t <- pTerm
                             return (Pow f t)  
                            ||| return f

