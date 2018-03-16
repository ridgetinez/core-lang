
module PrettyPrinter where

import CorePrelude
import Language

-- |Handle pretty formatting of list of Core Supercombinators
pprint :: CoreProgram -> String
pprint [] = "\n"
pprint (x:xs) = (pprScmb x) ++ "\n" ++ (pprint xs)

-- |Handle flattening supercombinator definitions to Strings
pprScmb :: CoreScDefn -> String
pprScmb (id, args, e) = id ++ (pprArgs args) ++ (pprExpr e) 

-- |Flatten all elements into a string with spaces between elements
pprArgs :: Show a => [a] -> String
pprArgs []   = ""
pprArgs (x:xs) = (show x) ++ " " ++ (pprArgs xs)

-- |Handle pretty formatting of Core Expressions
pprExpr :: CoreExpr -> String
pprExpr (Evar v)  = v
pprExpr (ENum n)  = show n
pprExpr (EAp a b) = (pprExpr a) ++ " " ++ (pprAExpr b)

-- |Handle potential atomic expressions, reduce expr otherwise
pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = "(" ++ pprExpr e ++ ")"
