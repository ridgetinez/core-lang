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

parse :: String -> CoreProgram
parse = syntax . (\z -> map snd (clex 1 z))

-- | Takes in current line number and file contents
--   And break into tokens according to our grammar.
--   We ignore all whitespace, consider digits, and variables as
--   letter prefixed contiguous characters, comments by ||
--   otherwise, everything is matched as a single character.
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

-- | Take in a string and match for tokens that contain that string,
--   assuming that tokens are just strings
pLit :: String -> Parser String
plit s []     = []
pLit s (t:ts) 
  | s == t    = [(s, ts)]
  | otherwise = [("", ts)]

testParser :: String -> (Parser a) -> [(a, [Token])]
testParser s p = p (map snd $ clex 1 s)

syntax :: [Token] -> CoreProgram
syntax = error "unimplemented!"
