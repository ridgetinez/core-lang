module MemHeap(Heap, Addr, hInitial, hAlloc, hUpdate, hFree) where

import Data.List
import language

-- (numAddrOccupied, remainingAddr, coupledPairs)
-- potential for overflow, as
-- 		req. address exceeds 2(2^32-1), overwriting addr. 1
-- just like malloc, a solution would be to give a hNULL
data Heap a = (Int, [Addr], [(Addr, a)])

type Addr = Int

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr) -- seems like the state monad, the runState
hAlloc (n, (next:rest), addrs) x = ((n+1, rest, (next, x):addrs), next)

hUpdate :: Heap a -> Addr -> a -> Heap a -- seems like get
hUpdate (n, free, addrs) a x = case (lookup a addrs) of
	Just b  -> (n, free, (:) (a,x) $ remove (a,b) addrs)
	Nothing -> (n, free, addrs)

-- i seem to be doing a ton of pattern matching on this heap hMM, MONADS?
hFree :: Heap a -> Addr -> Heap a -- seems like put
hFree (n, free, addrs) a = case lookup a addrs of
	Just b  -> (n-1, a:free, remove (a,b) addrs)
	Nothing -> (n, free, addrs)

hLookup :: Heap a -> Addr -> Maybe a
hLookup (n, free, addrs) a = lookup a addrs

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, addrs) = getBindersOf addrs

hSize :: Heap a -> Int
hSize (n, _, _) = n

