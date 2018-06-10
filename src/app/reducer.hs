module Reducer where

import Parser

{-
  Execution ~ Evaluation of expressions, where we model expressions as graphs.
  Each node in the graph is a reducible expression, where evaluation is equivalent
  to reducing each 'redex', only stopping when we have no more redexes (normal form)
  The order we perform reduction produces the same normal form (Simon,1987) however 
  our graph may be infinite, and in this case will fail to terminate.

  Normal order reduction reduces the most 'outer' application first, and then
  combines up by reduction of primitives.
-}

-- state for our eval state machine
-- @TiStack   - memory addresses of all
-- @TiDump    - stacks of TiStack to keep track of prev while recursing through graph
-- @TiHeap    - lol
-- @TiGlobals - lol
-- @TiStats   - execution statistics for the lulz
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

runProg :: [Char] -> [Char]
runProg = showResults . eval . compiler . parse

-- translate program into form for reduction (~ execution)
compile :: CoreProgram -> TiState
compile = undefined

-- perform reduction, returning all the states reduction went through
eval :: TiState -> [TiState]
eval = undefined

-- format results for printing
showResults :: [TiState] -> [Char]
showResults = undefined
