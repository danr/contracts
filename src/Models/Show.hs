{-# LANGUAGE RecordWildCards,ScopedTypeVariables,ExplicitForAll #-}
{-

    Printing a model. Random thoughts about printing models:

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

import Control.Monad

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
    arity :: t -> Arity
    arity = Arity . length . fst . split

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

-- | Environment needed is a map from the strings in the model to types
showModel :: forall t . Typelike t => Bool -> Bool -> TyLookup t -> Model -> String
showModel ignore_ty typed_metas ty_lookup Model{..} =

    let min_set,cf :: Elt -> Bool
        [min_set,cf] =
            [ let pred_map = M.fromList
                      [ (d,b)
                      | Predicate p' tbl <- predicates
                      , p' == p
                      , ([d],b) <- tbl
                      ]
              in \ x -> fromMaybe (error $ "showModel, missing entry for "
                                                ++ show x ++ " in " ++ show p)
                                  (M.lookup x pred_map)
            | p <- [Min,CF]
            ]

        -- NB: multiset, an elt can have different representations at
        -- different types.
        reprs :: [(Elt,ConRepr)]
        reprs = constructorReprs functions

        -- The app function
        app_map = fromMaybe (error "no app table?") . listToMaybe $
                [ M.fromList tbl | Function App tbl <- functions ]

        app :: Elt -> Elt -> Elt
        app x y = fromMaybe (error "app") (M.lookup [x,y] app_map)

        -- Looking up in the ty environment
        lookup_ty :: String -> String -> t
        lookup_ty u s = fromMaybe (error $ "lookup_ty " ++ u ++ ", miss: " ++ s)
                                  (M.lookup s ty_lookup)


        skolems :: [(String,Elt,t)]
        skolems =
            [ (s,e,lookup_ty "skolem" s)
            | Function (Skolem s) [([],e)] <- functions
            ]

        ptrs :: [(String,Elt,t)]
        ptrs =
            [ (s,e,lookup_ty "ptrs" s)
            | Function (Pointer s) [([],e)] <- functions
            ]

        unspun_pointers :: [Function]
        unspun_pointers =
            [ Function (Fun sk) (spinner dom_size a e app)
            | (sk,e,t) <- skolems
            , let a@(Arity n) = arity t
            , n > 0
            ]

        elt_repr = eltRepr ignore_ty ty_lookup typed_metas skolems ptrs min_set reprs

        show_funs fs =
            [ f_cnc
            | fun@(Function (Fun f) _) <- fs
            , let f_cnc = showFunTbl (elt_repr True) min_set cf fun
                                     (lookup_ty "functions" f)
            ]

        header h s
            | null s    = []
            | otherwise = [h,""] ++ s

    in  unlines $
            header "Skolems:"
                [ showStrTbl [([],(cf e,show sk_repr))] sk t
                | (sk,e,t) <- skolems
                , arity t == 0
                , let sk_repr = elt_repr False e t
                ] ++
            header "Skolem pointers:" (show_funs unspun_pointers) ++
            header "Functions:" (show_funs functions)

data Repr
    = Con String [Repr]
    | Uninteresting
    | Var String
    | Meta Int (Maybe String)

instance Show Repr where
    showsPrec d r = case r of
        Uninteresting        -> showString "..."
        Var s                -> showString (showOp s)

        Meta i Nothing       -> showChar '?' . showsPrec d i
        Meta i (Just ty_str) -> showParen (d >= 10) $
            showChar '?' . showsPrec d i . showString " :: " . showString ty_str

        Con s as -> showParen (d >= 10 && not (null as)) $
            foldl1 (\u v -> u . showChar ' ' . v)
                   (put (map (showsPrec 10) as))
          where
            put :: [ShowS] -> [ShowS]
            put [x,y] | isOp s = [x,showString s,y]
            put xs = showString s:xs

-- | Find a good representation for an element at some type
--
--   QUESTION: Do we always want to print as a constructor rather than
--   a skolem variable? Then instead of this:
--       x = Zero
--       y = Succ x
--   we get:
--       y = Succ Zero
--
--   But what about u = Succ w, and w = Succ u... Then we will get
--   u = Succ (Succ u) and w = Succ (Succ w) and the relationship is lost.
--
--   Oh... We probably want to print it as as constructor if there is a nullary one
eltRepr :: forall t . Typelike t
        => Bool
        -- ^ Ignore type information
        -> TyLookup t
        -- ^ A mapping from Strings to types
        -> Bool
        -- ^ Write types of metas
        -> [(String,Elt,t)]
        -- ^ Skolem representations
        -> [(String,Elt,t)]
        -- ^ Pointers representations
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
        -> Repr
eltRepr ignore_ty ty_lookup typed_metas skolems ptrs min_set reprs
    = ((head .) .) . go []
  where
    go :: [(Elt,t)] -> Bool -> Elt -> t -> [Repr]
    go visited as_skolem_ok e@(Elt d) ty =
        -- If it is not interesting, print it as _
        [ Uninteresting | not (min_set e) ] ++

        -- Try to print it as a nullary constructor
        [ Con c []
        | (e',ConRepr c []) <- reprs
        , e' == e
        , let con_ty = fromMaybe (error $ "eltRepr nullary ty con lookup: " ++ c)
                                 (M.lookup c ty_lookup)
        , ignore_ty || con_ty `lg` ty
        ] ++

        -- Try to print it as a skolem variable if that's ok
        [ Var s
        | as_skolem_ok
        , (s,e',ty') <- skolems
        , e == e'
        , ignore_ty || eqTy ty ty'
        ] ++

        -- Try to print it as a pointer
        [ Var p
        | (p,e',ty') <- ptrs, e == e'
        , ignore_ty || eqTy ty ty'
        ] ++

        -- Try to print it as a constructor with arguments
        -- In the recursive case it's OK to write it as a skolem variable again
        [ Con c arg_reprs
        | null (filter (\(e',ty') -> e' == e && eqTy ty' ty) visited)
        -- ^ Don't revisit when priting as a constructor
        , (e',ConRepr c es) <- reprs
        , length es > 0
        , e' == e
        , let con_ty = fromMaybe (error $ "eltRepr ty lookup: " ++ c)
                                 (M.lookup c ty_lookup)
              (es_tys,res_ty) = peel (Arity (length es)) con_ty
        , ignore_ty || res_ty `lg` ty
        , let es_tys' | ignore_ty  = es_tys
                      | otherwise  = map (unifySubst res_ty ty) es_tys
        , arg_reprs <- zipWithM (go ((e,ty):visited) True) es es_tys'
        ] ++

        -- No reasonable information, print it as a metavariable,
        -- attached with type information if desired
        [ Meta d (guard typed_metas >> return (showTy ty)) ]

type FunTblRepr = [([Repr],Repr)]

showFunTbl :: Typelike t
           => (Elt -> t -> Repr)
           -- ^ How to represent an element (from eltRepr)
           -> (Elt -> Bool)
           -- ^ Min set
           -> (Elt -> Bool)
           -- ^ CF
           -> Function
           -- ^ The function
           -> t
           -- ^ The type of the function
           -> String
showFunTbl elt_repr min_set cf fun ty =
    let Function (Fun f) tbl = fun
        (ty_args,res_ty) = peel (lambdaArity fun) ty

        show_arg e t = showsPrec 10 (elt_repr e t) ""

        show_res e t = show (elt_repr e t)

        str_tbl = [ (zipWith show_arg args ty_args,(cf res,show_res res res_ty))
                  | (args,res) <- tbl
                  , min_set res ]

    in  showStrTbl str_tbl f ty

showStrTbl :: Typelike t => [([String],(Bool,String))] -> String -> t -> String
showStrTbl str_tbl f ty =
    let args_lengths :: [Int]
        args_lengths = map (maximum . map length) (transpose args)
          where
            args :: [[String]]
            args = fst (unzip str_tbl)

        pad :: String -> Int -> String
        pad s x = s ++ replicate (x - length s) ' '

        showCF True  = ' '
        showCF False = '#'

        f' = showOp f

        -- Put the function in the middle if a binary infix function
        mid [x,y] | isOp f = [x,f,y]
        mid xs             = f':xs

        -- But then, remove surrounding parenthesis - a bit of a hack ;)
        -- (used to do this, but then the alignment gets wrong, bleh)
        _trim ('(':x:xs) | last (x:xs) == ')' = init (x:xs)
        _trim xs = xs

    in  indent $
            (f' ++ " :: " ++ showTy ty) :
            [ intercalate " "
                (mid (zipWith pad args args_lengths) ++ ['=':showCF cf:"",res])
            | (args,(cf,res)) <- str_tbl
            ]

indent :: [String] -> String
indent = unlines . map ("    " ++)

showOp :: String -> String
showOp xs | isOp xs   = "(" ++ xs ++ ")"
          | otherwise = xs

isOp :: String -> Bool
isOp = any (`elem` "!#$%&*+./<=>?@|\\^-~:")

