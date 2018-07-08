module Language where

-- language for the CORE expressions (can be evaluated)
data Expr a
  = Evar String                       -- (terminal) variables
  | ENum Int                          -- (terminal) num values
  | EConstr Int Int                   -- (terminal) data constructors <uid,arity>
  | EAp (Expr a) (Expr a)             -- function application
  | ELet                              -- let bindings
      IsRec                           -- recursive flag
      [(a, Expr a)]                   -- [name, expr] bindings
      (Expr a)                        -- let-body
  | ECase                             -- pattern-matching case
      (Expr a)                        -- usually a package to pattern match
      [Alter a]                       -- alternative expr
  | ELam [a] (Expr a)                 -- lambda expressions [params] -> expr
  deriving Show

{-
what are the benefits of the above implementation and constructing types below?
data Expr (a :: *)   -- need to import KINDSIGNATURES for this to work
  = Evar :: String -> Expr String
  = Enum :: Int    -> Expr Int
  = EAp  :: Expr a -> Expr a -> Expr a 
GADT allows us to define postconditions on the type of our Expr constructors
-}

type Program a   = [ScDefn a]
type CoreProgram = Program String
type ScDefn a    = (String, [a], Expr a)
type CoreScDefn  = ScDefn String
type Alter a     = (Int, [a], Expr a)
type CoreAlt     = Alter String
type CoreExpr    = Expr String

type IsRec = Bool
recursive    :: IsRec
nonRecursive :: IsRec
recursive     = True
nonRecursive  = False

isAtomicExpr :: CoreExpr -> Bool
isAtomicExpr (Evar e) = True
isAtomicExpr (ENum e) = True
isAtomicExpr e        = False

-- |Extracts binders and sub-exprs from list of definition expressions
bindersOf :: [(a,b)] -> [a]
bindersOf defns = [symbol | (symbol, _) <- defns]

exprsOf :: [(a,b)] -> [b]
exprsOf defns = [expr | (_, expr) <- defns]
