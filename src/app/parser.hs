module Parser where

import CorePrelude
import Language
import ExpSeq
import PrettyPrinter
import Data.Char

type Token = String
type Parser a = [Token] -> [(a, [Token])]
-- Our parser should return a list of tokens left to be parsed,
-- i.e. we want to seq. compose two parsers A,B on tokens. By applying
-- A(tokens) we'd want to at least return the parsed value and [Token] left.
-- Secondly, it might be impossible/difficult for our parser to generate
-- the exact meaning of the current token (issues with lookahead). We solve this
-- case by return a list of potential matches to our parse, hence the
-- [(a,[Token])]
--


parse :: String -> CoreProgram
parse = syntax . (\z -> map snd (clex 1 z))

-- | Takes in current line number and file contents
--   And break into tokens according to our grammar.
clex :: Int -> String -> [(Int, Token)]
clex _ []     = []
clex i (c:cs)
  | c == '\n' = clex (i+1) cs
  | isSpace c = clex i cs
  | isDigit c = (token nump) : clex i (restcs nump) -- match on digits
  | isAlpha c = (token varp) : clex i (restcs varp) 
  | c == '|' && (head cs) == '|' = clex (i+1) (restcs (\x -> x /= '\n')) 
  | [c,(head cs)] `elem` twoCharOps = (i,[c,(head cs)]) : clex i (tail cs)
  | otherwise = (i, [c]) : clex i cs
  where
    twoCharOps = ["==", "~=", ">=", "<=", "->"]
    varp       = \x -> isAlpha x || isDigit x || x == '_'
    nump       = isDigit
    token  p   = (i, c : takeWhile p cs) -- keep taking digits
    restcs p   = dropWhile p cs          -- drop digits from head

-- | Take the first parse which has no tokens left, else syntax error
syntax :: [(Int, Token)] -> CoreProgram
syntax = takeFirstParse . pCoreProgram
  where
    takeFirstParse ((prog, [])) : others) = prog
    takeFirstParse (parse       : others) = takeFirstParse others
    takeFirstParse other                  = error "Syntax error!"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    -- | Take the first set of parameters such that all are included in parse
    mkSc name []     _ expr = (name, [], expr)
    mkSc name (t:ts) _ expr
      | head (snd t) == "=" = (name, fst t, expr)
      | otherwise = mkSc name ts _ expr

-- question here, how do I match on all forms of Expr?
pExpr :: Parser CoreExpr
pExpr = pAlt pLet $ pAlt pCase $ pLambda pAExpr

pLet :: Parser CoreExpr
pLet = pThen4 f (pAlt (pLit "let") (pLit "letrec"))
                (pOneOrMoreWithSep pDefn (pLit ";"))
                (pLit "=")
                pExpr

pCase :: Parser CoreExpr
pCase = pThen4 f (pLit "case")
                 pExpr
                 (pLit "in")
                 (pOneOrMoreWithSep pCaseAlt (pLit ";"))

pLambda :: Parser CoreExpr
pLambda = pThen4 f (pLit "\\")
                   (pOneOrMore pVar)
                   (pLit ".")
                   pExpr 

pAExpr :: Parser CoreExpr
pAExpr = pAlt (pApply pVar Evar) (pApply pNum ENum)

pDefn :: Parser 

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

-- | Abstraction over comparison parsers
pSat :: (String -> Bool) -> Parser String
pSat f []     = []
pSat f (t:ts)
  | f t = [(t, ts)]
  | otherwise = []

-- | Take in a string and match for tokens that contain that string,
--   assuming that tokens are just strings
pLit :: String -> Parser String
pLit s = pSat (== s) 

-- Recognises all tokens who's heads are alphabetical EXCEPT our core keywords
pVar :: Parser String
pVar = pSat (\t -> isAlpha (head t) && not (elem t keywords))

pNum :: Parser Int
pNum = pApply (pSat isNum) read
  where
    isNum []     = True
    isNum (c:cs) = isDigit c && isNum cs 


-- | Given two parsers that match same type, apply bothn
--   parsers on tokens and concatenate results. Is a | OR in a BNF grammar
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 ts = (p1 ts) ++ (p2 ts)

-- | Given two parsers, sequentially compose the parsers, and combine their
--   matches. Corresponds to the ... sequencing symbols in a BNF grammar
--   @args f - function which combines the result of the parse
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen comb p1 p2 ts = [(comb v1 v2, ts2) | (v1, ts1) <- p1 ts,
                                           (v2, ts2) <- p2 ts1]

-- |If possible, we can change the combinator to be type (a->b->c)
-- and then define these in terms of pThen only. This sacrifices
-- Needing to do a quadruple function application, but you could always
-- do it afterwards (little rough to unravel structure that function packages
-- all these values into)
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 comb p1 p2 p3 ts = [(comb v1 v2 v3, ts3) | (v1, ts1) <- p1 ts,
                                               (v2, ts2) <- p2 ts1,
                                               (v3, ts3) <- p3 ts2]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 comb p1 p2 p3 p4 ts = [(comb v1 v2 v3 v4, ts3) | (v1, ts1) <- p1 ts,
                                                     (v2, ts2) <- p2 ts1,
                                                     (v3, ts3) <- p3 ts2,
                                                     (v4, ts4) <- p4 ts3]

-- |Acts as the star operator in a BNF grammar, match on a list of tokens
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pAlt (pOneOrMore p) (pEmpty [])

-- | Doesn't seem to work for pLit parsers
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

-- |Given a default value to return, pEmpty doesn't remove any tokens from list
pEmpty :: a -> Parser a
pEmpty s ts = [(s, ts)]

-- | effectively this is an fmap occurrence over the values matched. Functor?
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f ts = [(f x, xs) | (x, xs) <- p ts]

-- | When the whole string can match on this parser, it seems to say that pLit
-- has non exhaustive patterns, I have no idea though.
-- pOneOrMoreWithSep p1 p2 = pApply (pOneOrMore $ pThen (,) p1 p2) (\xs -> map (\y -> fst y) xs)
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pThen (\x y -> y) p2 (pZeroOrMore p1))

testParser :: String -> (Parser a) -> [(a, [Token])]
testParser s p = p (map snd $ clex 1 s)

syntax :: [Token] -> CoreProgram
syntax = error "unimplemented!"
