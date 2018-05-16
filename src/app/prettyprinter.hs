
module PrettyPrinter where

import CorePrelude
import Language
import ExpSeq

-- |Helpers for creating expressions in testA
-- |Apply x n times on EAp.
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
                    where
                        e2s = e2 : e2s   -- infinite list of e2

-- |Handle pretty formatting of list of Core Supercombinators

pprint :: CoreProgram -> String
pprint prog =  expDisplay $ pprProgram prog

pprProgram :: CoreProgram -> ExpSeq
pprProgram []    = expNil
pprProgram scmbs = expInterleave expNewline $ map pprScmb scmbs

{-
pprint :: CoreProgram -> ExpSeq
pprint [] = expNewline
pprint xs = expInterleave (map (\x -> pprScmb x) xs)
-}

-- |Handle flattening supercombinator definitions to Strings
pprScmb :: CoreScDefn -> ExpSeq
pprScmb (id, args, e) = expConcat [strToExp id, strToExp " ", pprArgs args, strToExp " = ", pprExpr e]

-- |Flatten all elements into a string with spaces between elements
pprArgs :: [String] -> ExpSeq
pprArgs xs = expInterleave (strToExp " ") $ map (\x -> strToExp x) xs

-- |Handle pretty formatting of Core Expressions
pprExpr :: CoreExpr -> ExpSeq
pprExpr (Evar v)  = strToExp v
pprExpr (ENum n)  = strToExp (show n)
pprExpr (EAp a b) = expConcat [(pprExpr a), strToExp " ", (pprAExpr b)]
pprExpr (ELet isrec defns expr)
  = expConcat [strToExp keyword, expNewline,
               expIndent, pprDefns defns, expNewline,
               strToExp "in ", pprExpr expr, expNewline]
    where
    keyword | not isrec = "let"
            | otherwise = "letrec"
pprExpr (ECase e as)
  = expConcat [strToExp "case", strToExp " ", pprExpr e,
               strToExp " ", strToExp "of", expNewline,
               expIndent, pprAlts as]

pprAlts :: [CoreAlt] -> ExpSeq
pprAlts as = expConcat $ map pprAlt as

pprAlt :: CoreAlt -> ExpSeq
pprAlt (tag, ids, e) = expConcat [strToExp "<", strToExp (show tag), strToExp ">",
                                  expInterleave (strToExp " ") $ map strToExp ids,
                                  strToExp " -> ", pprExpr e]

-- |Handle pretty printing of let binding definitions
pprDefns :: [(String, CoreExpr)] -> ExpSeq
pprDefns defns = expInterleave sep (map pprDefn defns)
                 where
                 sep = expConcat [strToExp ";", expNewline, expIndent]

pprDefn :: (String, CoreExpr) -> ExpSeq
pprDefn (id, e) = expConcat [strToExp id, strToExp " = ", pprExpr e]


-- |Handle potential atomic expressions, reduce expr otherwise
-- NOTE: Repeated abuse of concatenation with increasingly larger strings
-- breeds quadratic running time from experimentation.
-- TODO: Make concatentation of any string length in O(n)
pprAExpr :: CoreExpr -> ExpSeq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = expConcat [strToExp "(", pprExpr e, strToExp ")"]
