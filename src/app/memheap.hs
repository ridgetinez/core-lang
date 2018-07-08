{-# LANGUAGE RankNTypes #-}

module MemHeap(Heap, Addr, hInitial, hAlloc, hUpdate, hFree, hLookup) where

import Data.List
import Language

{-
	We represent our heap as an infinite list, in practice
	we'll eventually run out of memory if we run into an infinite
	recursion that allocates to the heap.
-}

-- (numAddrOccupied, remainingAddr, coupledPairs)
-- potential for overflow, as
-- 		req. address exceeds 2(2^32-1), overwriting addr. 1
-- just like malloc, a solution would be to give a hNULL
type Heap a = (Int, [Addr], [(Addr, a)])
type Addr = Int

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr) -- seems like the state monad, the runState
hAlloc (n, (next:rest), addrs) x = ((n+1, rest, (next, x):addrs), next)

hUpdate :: Heap a -> Addr -> a -> Heap a -- seems like get
hUpdate (n, free, addrs) a x = case (lookup a addrs) of
  Just b  -> (n, free, (:) (a,x) $ freeAddr addrs a)
  Nothing -> (n, free, addrs)

-- i seem to be doing a ton of pattern matching on this heap hMM, MONADS?
hFree :: Heap a -> Addr -> Heap a -- seems like put
hFree (n, free, addrs) a = case lookup a addrs of
  Just b  -> (n-1, a:free, freeAddr addrs a)
  Nothing -> (n, free, addrs)

hLookup :: Heap a -> Addr -> Maybe a
hLookup (n, free, addrs) a = lookup a addrs

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, addrs) = bindersOf addrs

hSize :: Heap a -> Int
hSize (n, _, _) = n

-- helpers for address list manipulation
freeAddr :: Eq a => [(a,b)] -> a -> [(a,b)]
freeAddr [] _ = []
freeAddr (x:xs) addr
  | (fst x) == addr = freeAddr xs addr
  | otherwise = x : (freeAddr xs addr)

