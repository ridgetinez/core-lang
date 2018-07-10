module Reducer where

import Language
import Parser
import MemHeap
import CorePrelude

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

type TiStack = [Addr]							-- the spine stack of heap addresses
data TiDump = DummyTiDump 						-- don't need for our template reducer
type TiHeap = Heap Node 						-- tagged nodes are linked with addrs
data Node = NAp Addr Addr 						-- Application
		  | NSComb String [String] CoreExpr 	-- Supercombinator
		  | NNum Int 							-- Number literal
type TiGlobals = [(String, Addr)]				-- key value (supercomb. name, heap addr.)

-- Statistics definition, should make this an ADT when we boost tf out of this package
type TiStats = Int

tiStateInitial = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, globals, stats)
	= (stack, dump, heap, globals, f stats)


-- Program evaluation!
runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse

extraPreludeDefs = []
initialTiDump = DummyTiDump
initialStats = tiStateInitial


-- translate program into form for reduction (~ execution)
compile :: CoreProgram -> TiState
compile prog = (initialStack, initialTiDump, initialHeap, globals, initialStats)
  where
  	scDefs = prog ++ preludeDefns ++ extraPreludeDefs
  	(initialHeap, globals) = buildInitialHeap scDefs
  	initialStack = [mainAddress]
  	mainAddress = case lookup "main" globals of 
  	  Nothing -> error "main is not defined!"
  	  Just a  -> a

buildInitialHeap :: CoreProgram -> (TiHeap, TiGlobals)
buildInitialHeap scs = mapAccumulate (\(h,g) (s,vars,exp) -> 
                                        let (nextHeap, addr) = hAlloc h (NSComb s vars exp)
                                        in (nextHeap, (s,addr):g))
                                     (hInitial,[])
                                     scs

mapAccumulate :: (a -> b -> a) -> a -> [b] -> a
mapAccumulate f acc []     = acc
mapAccumulate f acc (x:xs) = mapAccumulate f (f acc x) xs 


-- perform reduction, returning all the states reduction went through
eval :: TiState -> [TiState]
eval state = state : restOfStates
  where
    restOfStates
      | tiFinal state = []
      | otherwise = eval nextState
    nextState = doAdmin $ step state

tiFinal :: TiState -> Bool
tiFinal ([x],d,h,g,s) = case hLookup h x of 
                          Just n  -> isDataNode n
                          Nothing -> False
tiFinal ([],_,_,_,_)  = error "empty stack!"    -- should use typechecker to guarantee never empty
tiFinal _             = False

-- will make more robust in future upgrades
isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state = case hLookup heap (head stack) of
  Just n  -> dispatch n
  Nothing -> error "Supercombinator not defined on the heap!"
  where
    (stack,dump,heap,globals,stats) = state
    dispatch (NNum x)    = error "Number literal being applied as a function!"
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSComb n vs e) = scStep state n vs e

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack,dump,heap,global,stats) a1 a2 =
  (a1:stack,dump,heap,global,stats)

scStep :: TiState -> String -> [String] -> CoreExpr -> TiState
scStep (stack,dump,heap,global,stats) name vars body = 
  (newStack,dump,newHeap,global,stats)
    where
      newStack = initialisedAddress : (drop (length vars + 1) stack)
      (newHeap,initialisedAddress) = instantiate body heap env
      env = argBindings ++ global
      argBindings = zip vars $ getScArgs stack heap

-- precondition: NSComb address is at the top of the stack
getScArgs :: TiStack -> TiHeap -> TiStack
getScArgs (sc:stack) heap -- we want all the addresses of the arguments of our sc
  = map go stack where go addr = arg where Just (NAp f arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> [(String,Addr)] -> (TiHeap,Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env  = hAlloc heap2 (NAp a1 a2)
  where
    (heap1,a1) = instantiate e1 heap env
    (heap2,a2) = instantiate e2 heap1 env  -- thread heap through sequentially!
instantiate (Evar v) heap env = case lookup v env of
  Just a -> (heap, a)  -- this address points to either a global, or one of the arguments applied to this function
                       -- back in getScArgs, this is like pointer direction!
  Nothing -> error "Local variable in function has no defined value!"



doAdmin :: TiState -> TiState
doAdmin (stack,dump,heap,global,stats) = (stack,dump,heap,global,tiStatIncSteps $ tiStatGetSteps stats)

-- format results for printing
showResults :: [TiState] -> [Char]
showResults = undefined
