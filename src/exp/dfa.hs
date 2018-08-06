
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE  RankNTypes, PolyKinds #-}

module DFA where

-- suppose we had a state machine (n, m, d, t)
-- with transition rules (n, 0, 0, t) --> terminate
--						 (n, m, 0, t) --> (n, m-1, n, t)
--                       (n, m, d, t) --> (n, m, d-1, t+1)
-- this machine does positive integer multiplication (proof of invariant NM = Nm + d + t in book)

type MultState = (Int, Int, Int, Int)   -- (n, m, d t)

{-
data Nat = Z | S Nat

type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance (+) n Z     = n
type instance (+) n (S m) = (S n) + m

type family (*) (n :: Nat) (m :: Nat) :: Nat
type instance (*) n Z     = Z
type instance (*) n (S m) = (n * m) + m

-- define property equality on Nat kinds

class EQ (m :: Nat) (n :: Nat) where
instance EQ Z Z
instance EQ m n => EQ (S m) (S n) -- if EQ m n, then EQ (S m) (S n)

-- now define our SafeMultState
-- could we do the whole state machine in the type level?
data SafeMultState (n :: Nat) (m :: Nat) (m' :: Nat) (d :: Nat) =
	EQ XXXX => XXXX

-}

-- can we make this evaluator type safe, i.e. encode the
-- invariant NM = Nm + d + t so that the type checker rejects
-- ill-formed tuples?
evalMult :: MultState -> [MultState]
evalMult state
  | final state = [state]
  | otherwise   = state : evalMult (transition state)

transition :: MultState -> MultState
transition (n, m, 0, t) = (n, m-1, m, t)
transition (n, m, d, t) = (n, m, d-1, t+1)
transition _            = error "ill-formed starting state!"

final :: MultState -> Bool
final (n, 0, 0, t) = True
final _            = False


