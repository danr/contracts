{-# LANGUAGE RecordWildCards,ScopedTypeVariables,ExplicitForAll #-}
{-

How do we print a value !1?

How do we print a value !1, with a type tau?

say we know this:

1 = Cons 2 3
2 = Cons 1 3
3 = Cons 3 1

We can make a lhs/rhs distinction.  A good lhs is either a skolem
variable, or a nullary constructor. If the lhs is not nullary,
then the rhs is a constructor of values, which generally should have their lhs
printed. If no such alternative exist,
just print ?3 for some meta-variable.

something like this:
    data Value = Nullary String | Con String [Value] | Skolem String | Meta Elt

We should probably prefer skolems to nullary constructors, if we
can. Or?

Also dump a list of the rhs of all skolems: they constitute the counterexample.
If they contain metas, print their rhs as well.
Maybe we can do this smarter:

    xs = Cons Nothing ?2
    ?2 = Cons (Just ?3) xs

We can simply print:

    xs = Cons Nothing (Cons (Just ?3 xs)

Which is a bit easier to survey. Typically counterexamples will have
lots of such "loops".  Maybe with trees we get weird things:

    t = Branch ?1 ?4 ?2
    ?1 = Branch t ?5 ?2
    ?2 = Branch t ?6 t

How do we print this?

    t = Branch (Branch t ?5 (Branch t ?6 t)) ?4 (Branch t ?6 t)

So I guess if we want to write a skolem, then we can always go down
metas untill we find another skolem.

    t = Branch ?1 ?4 ?2
    ?1 = Branch t ?5 ?2
    ?2 = Branch t ?6 t
    u = Branch t ?6 t

This can be printed:

    t = Branch (Branch t ?5 u) u
    u = Branch t ?6 t

Which might be easier than the original. Maybe the heuristic should be
that if we have the same meta variable (at the same type) at more than
one occurence, we make a new name for it. So the original example,
without u, could be written like this:

    t = Branch (Branch t ?5 t') t'
    t' = Branch t ?6 t

Where t' is just a generated name from t.

Then when we write function tables, we should maybe want to use lhs
only, i.e. rather than writing

f (Branch (Branch t ?5 t') t') = v,

we just simply write

f t = v

But for (Branch t ?5 t') we have no skolem variable, so we better write it out

f (Branch t ?5 t') = v2

The left hand sides of functions should probably also be lhs. Don't write out
constructors if you can.

The question is when a skolem variable is a nullary constant. But we
probably still want to write the skolem variable.


What about types?? Say we have an element

    ?1 = Cons ?2 ?3
    ?1 = Branch ?1 ?5 ?3

Then ?1 can be printed both as List a and Tree b, for any a and b.

Oh, we should make UNR and BAD have any type.

So given all possible ways to write elements, and a given type, we
need to find a way to write this element, and also suggest how its
recursive components should be typed.

In the end of the day, we want to have, for all skolem variables c,
a way to print them.... oh. yes, they can only have one type.
We can also invent new skolem names for its components.

I guess the interesting part is when a skolem variable is a
function. Then we don't try to find such a representation as above,
but rather print its function table.

    THE PROCEDURE OF PRINTING COUNTERMODELS

    1)  For a domain element d, check which constructors it is equal to.
        For a constructor K with arguments d1..dn, check if

            d = K d1 .. dn
            proj_1 d = d1
            ...
            proj_n d = dn

        Then we know that d = K d1 .. dn.
        We now do not need constructors & projections any more.

    2)  From the function table for app, we can spin around it to
        get a function table starting from any domain element as a
        pointer, and spin it around as many times as the arity of
        the function.

    3)  Find typed representatives for skolem variables.
        Each skolem variable has one unique type (obviously)
        (TODO: Set the correct type of these Vars!!)

        Variables that occur more than twice (with the same type)
        can be given new names.

    4)  Print the skolem variables' values, and the function tables.

How do we print the function table for some domain element that does
not have a skolem variable for some type? Well, then try to find a
constructor (a rhs) of the right type... Otherwise it's probably
something not min, and we can just write it as a metavariable.
(Maybe that's something to take into consideration... If the lhs of
the function is not min, then don't write this case)




    Questions

    When should we use maps and when should we use lists?
    Hmm... These are going to be so small so we could just as
    well use lists.



-}
module Models.Show where

import Models.Model
import Models.Spin


import qualified Data.Map as M
import Data.Map (Map)

import Data.Maybe
import Data.List

data ConRepr = ConRepr String [Elt]

-- | For a constructor K with arguments d1..dn, check if
--   @
--       d = K d1 .. dn
--       proj_1 d = d1
--       ...
--       proj_n d = dn
--   @
--   Then we know that d = K d1 .. dn.
constructorReprs :: [Function] -> [(Elt,ConRepr)]
constructorReprs tbls =
    [ (d,ConRepr con args)
    -- ^ It's established that d can be written as con(args).
    | Function (Constructor con) tbl <- tbls
    -- ^ Look through all constructor tables
    , (args,d) <- tbl
    -- ^ A row saying that con(args) = d
    , and (zipWith (\i di -> proj i con d == di) [0..] args)
    -- ^ Check that for each di in args, we have proj_i con d = di
    ]
  where
    proj :: Int -> String -> Elt -> Elt
    proj coord con d = proj_map M.! (coord,con,d)

    projs :: [((Int,String,Elt),Elt)]
    projs =
        [ ((coord,con,d),d_projected)
        | Function (Projection coord con) tbl <- tbls
        , ([d],d_projected) <- tbl
        ]

    proj_map = M.fromList projs

class Typelike t where
    -- | Type equality
    eqTy  :: t -> t -> Bool

    -- | Show type
    showTy :: t -> String

    -- | Peel off these many arguments (@splitFunTysN@)
    peel  :: Arity -> t -> ([t],t)

    -- | The "type-arity" (in constrast to the "lambda-arity") of a function
    --   (@length . fst . splitFunTys@)
    arity :: t -> Arity

    -- | Split a type into its arguments and result types (@splitFunTys@)
    split :: t -> ([t],t)
    split ty = peel (arity ty) ty

    -- | Less general or equal to (a partial order)
    --
    --   True example :  [a] `lg` [Nat]
    --   False example: Bool `lg` Maybe Nat
    --
    --   Given any concrete C & D, C `lg` D iff C == D
    --   Given any type variable a, a `lg` t for any type t
    --   (but what if t is some _other_ type variable?)
    --
    --   For any type constructor C, and arguments a1..an, and b1..bn
    --   we have that C a1 .. an `lg` C b1 .. bn <=> /\i ai `lg` bi
    lg :: t -> t -> Bool

    -- | @unifySubst r s t@ unifies the free variables in @r@ with those
    --   in @s@, and uses that substitution in @t@.
    --
    --   This function can only be used if @r `lg` s@ is true.
    --
    --   Example: @unifySubst [a] [nat] a@ = Nat
    --
    --   Use something like @tcMatchTy@ in @Unify@ to get the
    --   substitution and then @substTy@ in @Type@ to do the
    --   substitution.  The first two arguments to @tcMatchTy@ is the
    --   "template" and its arguments, which we want to unify with the
    --   third argument: the target.  Then we can probably implement
    --   lg in terms of that too.
    --   (What about UNR/BAD? they should have the AnyType)
    unifySubst :: t -> t -> t -> t

type TyLookup t = Map String t

-- | Environment needed
--
--       Map String t
--
showModel :: forall t . Typelike t => TyLookup t -> Model -> String
showModel ty_lookup Model{..} =

    let min_set_map = M.fromList
            [ (d,is_min)
            | Predicate Min tbl <- predicates
            , ([d],is_min) <- tbl
            ]

        min_set :: Elt -> Bool
        min_set x = fromMaybe (error "min_set") (M.lookup x min_set_map)

        -- NB: multiset, an elt can have different representations at
        -- different types.
        reprs :: [(Elt,ConRepr)]
        reprs = constructorReprs functions

        -- The app function
        app_map = fromMaybe (error "no app table?") . listToMaybe $
                [ M.fromList tbl | Function App tbl <- functions ]

        app :: Elt -> Elt -> Elt
        app x y = fromMaybe (error "app") (M.lookup [x,y] app_map)

        skolems :: [(String,Elt,t)]
        skolems = [ (s,e,fromMaybe (error $ "skolem type: " ++ s)
                                   (M.lookup s ty_lookup))
                  | Function (Skolem s) [([],e)] <- functions
                  ]

    in  unlines $
            ["Skolems:",""] ++
            concat
                [ ["    " ++ sk ++ " :: " ++ showTy t
                  ,"    " ++ sk ++ " = " ++ sk_elt
                  ,""]
                | (sk,e,t) <- skolems
                , let sk_elt = showElt ty_lookup skolems min_set reprs False e t
                ]

{-
Then print something like this:...

Skolems:

   w :: [Nat]
   w = ?1 = ?3 : ?1 = UNR : w

   ?3 is not min, (and is UNR)

   w :: [Nat]
   w = _ : w

   risers_concl w = ?5

Fixed point induction functions:

    Conclusion:


    Hypothesis:

Functions:



Functions as pointers:

-}

-- | Show an element!
showElt :: forall t . Typelike t
        => TyLookup t
        -- ^ A mapping from Strings to types
        -> [(String,Elt,t)]
        -- ^ Skolem representations
        -> (Elt -> Bool)
        -- ^ Min set
        -> [(Elt,ConRepr)]
        -- ^ Constructor representations
        -> Bool
        -- ^ Can we write this as a skolem variable?
        -> Elt
        -- ^ The element to show
        -> t
        -- ^ At which type to show it
        -> String
showElt ty_lookup skolems min_set reprs as_skolem_ok_init e ty
    = go {- [(e,ty)] -} [] as_skolem_ok_init e ty
  where
    go :: [(Elt,t)] -> Bool -> Elt -> t -> String
    go visited as_skolem_ok e@(Elt d) ty = head $ -- intercalate "/" $
        -- If it is not interesting, print it as _
        [ "_" | not (min_set e) ] ++

        -- Try to print it as a skolem variable if that's ok
        [ s | as_skolem_ok, (s,e',ty') <- skolems, e == e', eqTy ty ty' ] ++

        -- Try to print it as a pointer (TODO)
        [] ++

        -- Try to print it as a constructor
        -- In the recursive case it's OK to write it as a skolem variable again
        [ "(" ++ intercalate " " (c : zipWith (go ((e,ty):visited) True) es es_tys') ++ ")"
        | null (filter (\(e',ty') -> e' == e && eqTy ty' ty) visited)
        -- ^ Don't revisit when priting as a constructor
        , (e',ConRepr c es) <- reprs
        , e' == e
        , let con_ty = fromMaybe (error $ "showElt ty lookup: " ++ c)
                                 (M.lookup c ty_lookup)
              (es_tys,res_ty) = peel (Arity (length es)) con_ty
        , res_ty `lg` ty
        , let es_tys' = map (unifySubst res_ty ty) es_tys
        ] ++

        -- No reasonable information, print it as a metavariable
        [ '?' : show d ]


{-

-- Old stuff:

-- | Show an element at a particular type, priority:
--
--      * As a skolem variable with this type
--      * As a pointer with this type
--      * As a constructor with this type
--      * As a meta-variable
--
--   When printing things as a constructor, we must not
--   go into a loop...
showEltAtType :: Typelike t => Elt -> t -> String
showEltAtType (Elt d) ty =

-- | We will need a map from strings to these types (think varType . M.lookup)
showFunction :: Typelike t => Function -> Map String t -> t -> (Elt -> Bool) -> String
showFunction fn@(Function f tbl) ty min_set =
    [ unwords (show f : zipWith showEltAtType args arg_tys
                ++ ["=",showEltAtType res res_ty]
                ++ (guard (min_set res) >> "(min)"))
    | (args,res) <- tbl
    ]
  where
    (arg_tys,res_ty) = peel (arity fn) ty

showModel :: Int -> [Table] -> String
showModel size tbls = unlines $
    [ sym ++ " = " ++ val
    | i <- [1..size]
    , let sym = '!':show i
          val = showVal i
    , sym /= val
    ] ++
    "" :
    [ showSym sym ++ " = " ++ showVal i
    | Func sym [([],i)] <- tbls
    ] ++
    "" :
    [ twiggle b ++ show pred ++ "(" ++ showVal i ++ ")"
    | Pred pred tbl <- tbls
    , (i,b) <- tbl
    ]
  where
    showSym (OrigFunction s) = s
    showSym (Constructor s)  = s
    showSym (Skolem s)       = s
    showSym App              = "app"
    showSym (Pointer s)      = "ptr_" ++ s
    showSym (Projection i s) = "proj_" ++ show i ++ "_" ++ s

    twiggle True  = " "
    twiggle False = "~"

    showVal i = (intercalate "/" . map show) (Meta i : alts)
      where
        alts = (map snd . filter ((i ==) . fst)) reprs

    reprs = constructorReprs tbls

    -}
