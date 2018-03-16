
module PrettyPrinter where

import CorePrelude
import Language

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ fin []     = fin
-- foldr f fin (x:xs) = f fin (foldr f fin xs)
-- 
-- -- iteratively store result of first elements -> []
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl _ fin []     = fin
-- foldl f fin (x:xs) = foldl f (f fin x) xs

-- |Helpers for creating expressions in testA
-- |Apply x n times on EAp. 
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
                    where
                        e2s = e2 : e2s

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
-- NOTE: Repeated abuse of concatenation with increasingly larger strings
-- breeds quadratic running time from experimentation.
-- TODO: Make concatentation of any string length in O(n)
pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = "(" ++ pprExpr e ++ ")"
