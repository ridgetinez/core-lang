module ExpSeq where

-- (++) works in O(l) l = length of first arg -> O(n^2) on n concatenations
-- |ExpSeq implements concatenation in O(n) with definitions for keeping tab structure
-- |This is done by forgoing normal evaluation, instead evaluating all appends at
-- |calling expDisplay - this is my first soiree to lazy evaluation
data ExpSeq =
    ExpNil
  | ExpIndent
  | ExpNewline
  | ExpStr String
  | ExpAppend ExpSeq ExpSeq

-- expNil     :: ExpSeq                        -- Empty seq for equality
-- strToExp   :: String -> ExpSeq              -- String rep to exp seq
-- expAppend  :: ExpSeq -> ExpSeq -> ExpSeq    -- Append two expSeqs
-- expNewline :: ExpSeq                        -- Newline with indentation
-- expIndent  :: ExpSeq                        -- Pad left expSeqs with spaces
-- expDisplay :: ExpSeq -> String              -- Convert expSeq back to String

expNil     = ExpNil
strToExp s = ExpStr s
expIndent  = ExpIndent      -- not implemented
expNewline = ExpStr "\n"     -- not implemented

expAppend :: ExpSeq -> ExpSeq -> ExpSeq
expAppend x y = ExpAppend x y

flatten :: [ExpSeq] -> String
flatten []                   = ""
flatten ((ExpNil):xs)        = flatten xs
flatten ((ExpStr s):xs)      = s ++ flatten xs
flatten ((ExpNewline):xs)    = "\n" ++ flatten xs
flatten ((ExpIndent):xs)     = "\t" ++ flatten xs
flatten ((ExpAppend p q):xs) = flatten $ p : q : xs     --


expDisplay :: ExpSeq -> String
expDisplay e = flatten [e]

-- |Concatenate a list of ExpSeqs into one ExpSeq, no separators between
expConcat :: [ExpSeq] -> ExpSeq
expConcat exps = foldl expAppend expNil exps

-- |Between each element in an ExpSeq list, put another ExpSeq
expInterleave :: ExpSeq -> [ExpSeq] -> ExpSeq
expInterleave sep [x]    = x
expInterleave sep (x:xs) = expAppend x (g sep xs)
    where
        g sep []     = expNil
        g sep (x:xs) = sep `expAppend` (x `expAppend` g sep xs)
