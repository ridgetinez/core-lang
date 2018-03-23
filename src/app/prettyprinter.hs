
module PrettyPrinter where

import CorePrelude
import Language
import Exp

-- (++) works in O(l) l = length of first arg -> O(n^2) on n concatenations
-- |ExpSeq implements concatenation in O(n) with definitions for keeping tab structure
data ExpSeq = Int

-- expNil     :: ExpSeq                        -- Empty seq for equality
-- strToExp   :: String -> ExpSeq              -- String rep to exp seq
-- expAppend  :: ExpSeq -> ExpSeq -> ExpSeq    -- Append two expSeqs
-- expNewline :: ExpSeq                        -- Newline with indentation
-- expIndent  :: ExpSeq                        -- Pad left expSeqs with spaces
-- expDisplay :: ExpSeq -> String              -- Convert expSeq back to String

expAppend :: ExpSeq -> ExpSeq -> ExpSeq
expAppend x y = 

-- |Concatenate a list of ExpSeqs into one ExpSeq, no separators between
expConcat :: [ExpSeq] -> ExpSeq
expConcat xs = foldl expAppend expNil xs

-- |Between each element in an ExpSeq list, put another ExpSeq
expInterleave :: ExpSeq -> [ExpSeq] -> ExpSeq
expInterleave sep (x:y:xs) = expConcat [x, sep, y] (expInterleave xs)
expInterleave sep (x:xs)   = expConcat [sep, x] (expInterleave xs)
expInterleave sep []       = []

-- |Helpers for creating expressions in testA
-- |Apply x n times on EAp. 
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
                    where
                        e2s = e2 : e2s

-- |Handle pretty formatting of list of Core Supercombinators
pprint :: CoreProgram -> ExpSeq
pprint [] = expNewline
pprint xs = expInterleave (map (\x -> pprScmb x) xs)

-- |Handle flattening supercombinator definitions to Strings
pprScmb :: CoreScDefn -> ExpSeq
pprScmb (id, args, e) = expConcat [strToExp id, pprArgs args, pprExpr e]

-- |Flatten all elements into a string with spaces between elements
pprArgs :: Show a => [a] -> ExpSeq
pprArgs xs = expInterleave (strToExp ' ') (map expNil xs)

-- |Handle pretty formatting of Core Expressions
pprExpr :: CoreExpr -> ExpSeq
pprExpr (Evar v)  = strToExp v
pprExpr (ENum n)  = strToExp (show n)
pprExpr (EAp a b) = expConcat [(pprExpr a), strToExp " ", (pprAExpr b)]
pprExpr (ELet isrec defns expr)
  = expConcat [strToExp keyword, expNewline,
               strToExp " ", expIndent (pprDefns defns), expNewline,
               strToExp "in", pprExpr expr, expNewline]
    where
    keyword | not isrec = "let"
            | otherwise = "letrec"

pprDefns :: [(String, CoreExpr)] -> ExpSeq
pprDefns defns = expInterleave sep (map pprDefn defns)
                 where
                 sep = expConcat [strToExp ";", expNewline]

pprDefn :: (String, CoreExpr) -> ExpSeq 
pprDefn (id, e) = expConcat [strToExp id, strToExp " = ", expIndent (pprExpr e)]


-- |Handle potential atomic expressions, reduce expr otherwise
-- NOTE: Repeated abuse of concatenation with increasingly larger strings
-- breeds quadratic running time from experimentation.
-- TODO: Make concatentation of any string length in O(n)
pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = "(" ++ pprExpr e ++ ")"
