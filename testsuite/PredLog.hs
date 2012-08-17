module PredLog where

import Prelude(Bool(..),error)
import Contracts

data Formula
    = And [Formula]
    | Or  [Formula]
    | Implies (Formula) (Formula)
    | Neg (Formula)
    | Lit Bool

invariant :: Formula -> Bool
invariant f = case f of

    -- We should never have two consecutive negations
    Neg Neg{} -> False

    -- Binary operator list should be at least two elements, and their
    -- elements should brokenisfy the invariants
    And xs -> properList xs && all invariant xs
    Or xs  -> properList xs && all invariant xs

    -- None of the above? Recurse
    Implies x y -> invariant x && invariant y
    Neg x       -> invariant x

    -- Base case
    Lit x -> True

-- | Negating a formula
neg :: Formula -> Formula
neg (Neg f)         = f
neg (And fs)        = Or (map neg fs)
neg (Or fs)         = And (map neg fs)
neg (Implies f1 f2) = neg f2 `Implies` neg f1
neg (Lit b)         = Lit b

-- | What it means to retain a predicate
retain :: (a -> Bool) -> Contract (a -> a)
retain p = Pred p :-> \x -> Pred (\ r -> p x && p r)

-- | Invariant is crash free
all_cf = all ::: (CF --> CF) --> CF --> CF

-- | Invariant is crash free
invariant_cf = invariant ::: CF --> CF
  `Using` all_cf

{- -- This first one is broken but I cannot figure out why
-- | Map can retain a predicate
map_retains p = map ::: retain p --> retain (all p)
  `Using` (p ::: CF --> CF)
  `Using` all_cf

-- | Retaining is preserved by mapping
map_retains_invariant_by_lemma =
    map ::: retain invariant --> retain (all invariant)
  `Using` map_retains invariant
    -}

-- | Retaining is preserved by mapping
map_retains_invariant =
    map ::: retain invariant --> retain (all invariant)
  `Using` all_cf

map_retains_invariant_wo_all_cf =
    map ::: retain invariant --> retain (all invariant)

-- | Negating retains the invariant
neg_retains_invariant =
    neg ::: retain invariant
  `Using` map_retains_invariant_wo_all_cf

broken_neg_retains_missing_map_retains =
    neg ::: retain invariant
  `Using` invariant_cf
  `Using` all_cf

-- | Conjunction of a list
ands :: [Formula] -> Formula
ands []  = error "ands: Empty list"
ands [f] = f
ands fs  = And (concatMap flattenAnd fs)

-- | Flattening
flattenAnd :: Formula -> [Formula]
flattenAnd (And fs) = concatMap flattenAnd fs
flattenAnd f        = [f]

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

append_cf    = (++) ::: CF --> CF --> CF

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f (x:xs) = f x ++ concatMap f xs
concatMap f []     = []

concatMap_cf = concatMap ::: (CF --> CF) --> CF --> CF
  `Using` append_cf

broken_concatMap_cf_missing = concatMap ::: (CF --> CF) --> CF --> CF

flattenAnd_cf = flattenAnd ::: CF --> CF
  `Using` concatMap_cf

broken_flattenAnd_cf_missing = flattenAnd ::: CF --> CF

-- | Retaining is preserved by append
append_retains_invariant =
    (++) ::: (Pred (all invariant) :-> \xs
          -> Pred (all invariant) :-> \ys
          -> Pred (\rs -> all invariant xs && all invariant ys && all invariant rs))

-- | all p xs ++ all p ys = all p (xs ++ ys)
all_homomorphism = All (\p -> (++) ::: Pred (all p) --> Pred (all p) --> Pred (all p))

-- | Retaining is preserved by concat mapping
--   This one needs that all is a list homomorphism
concatMap_retains_invariant =
    concatMap ::: (Pred invariant :-> \x -> Pred (\rs -> invariant x && all invariant rs))
              --> retain (all invariant)
  `Using` append_retains_invariant
  `Using` all_homomorphism

broken_concatMap_retains_invariant =
    concatMap ::: (Pred invariant :-> \x -> Pred (\rs -> invariant x && all invariant rs))
              --> retain (all invariant)
  `Using` append_retains_invariant

broken_concatMap_retains_missing =
    concatMap ::: (Pred invariant :-> \x -> Pred (\rs -> invariant x && all invariant rs))
              --> retain (all invariant)

-- | Retaining is preserved by concat mapping
flattenAnd_retains_invariant =
    flattenAnd ::: (Pred invariant :-> \x
                -> Pred (\ rs -> invariant x && all invariant rs))
  `Using` concatMap_retains_invariant

broken_flattenAnd_retains_missing =
    flattenAnd ::: (Pred invariant :-> \x
                -> Pred (\ rs -> invariant x && all invariant rs))

{- Cannot get this to work right now, getting a
   countermodel where

     xs = And xs : xs
     all invariant xs = True
     invariant (ands xs) = False
     ands xs = xs = And (concatMap flattenAnd xs)

     concatMap flattenAnd xs = !4 = [] = Ands xs, hmm


ands_retains_invariant =
    ands ::: (CF :&: Pred nonEmpty :&: Pred (all invariant)
         :-> \ xs -> Pred (\r -> (all invariant xs && invariant r))
         )
  `Using` flattenAnd_retains_invariant
  `Using` concatMap_retains_invariant
  `Using` all_cf
  `Using` invariant_cf

     -}

-- * Auxiliary functions


map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x:map f xs
map f []     = []

all :: (a -> Bool) -> [a] -> Bool
all p (x:xs) = p x && all p xs
all p []     = True

any :: (a -> Bool) -> [a] -> Bool
any p (x:xs) = p x || any p xs
any p []     = False

properList :: [a] -> Bool
properList []  = False
properList [_] = False
properList _   = True

True  && b = b
False && _ = False

False || b = b
True  || _ = True

not True  = False
not False = True

nonEmpty []    = False
nonEmpty (_:_) = True

f . g = \x -> f (g x)

isAnd And{} = True
isAnd _     = False

isOr Or{} = True
isOr _    = False
