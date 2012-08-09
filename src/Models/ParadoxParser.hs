{-# LANGUAGE ViewPatterns #-}
{-

    Parsing models from paradox

    Typical input:

+++ BEGIN MODEL
% domain size is 4

'f_=='(!1,!1) = !1
'f_=='(!1,!2) = !1
'f_=='(!1,!3) = !1
'f_=='(!1,!4) = !3
'f_=='(!2,!1) = !2
'f_=='(!2,!2) = !3
'f_=='(!2,!3) = !3
'f_=='(!2,!4) = !3
'f_=='(!3,!1) = !3
'f_=='(!3,!2) = !3
'f_=='(!3,!3) = !3
'f_=='(!3,!4) = !3
'f_=='(!4,!1) = !3
'f_=='(!4,!2) = !3
'f_=='(!4,!3) = !3
'f_=='(!4,!4) = !2

a_ct_an = !4

a_ct_ao = !4

c_Cons(!1,!1) = !1
c_Cons(!1,!2) = !3
c_Cons(!1,!3) = !3
c_Cons(!1,!4) = !1
c_Cons(!2,!1) = !3
c_Cons(!2,!2) = !3
c_Cons(!2,!3) = !3
c_Cons(!2,!4) = !3
c_Cons(!3,!1) = !3
c_Cons(!3,!2) = !3
c_Cons(!3,!3) = !3
c_Cons(!3,!4) = !3
c_Cons(!4,!1) = !4
c_Cons(!4,!2) = !3
c_Cons(!4,!3) = !3
c_Cons(!4,!4) = !1

c_False = !1

c_Nil = !1

c_S(!1) = !1
c_S(!2) = !3
c_S(!3) = !3
c_S(!4) = !4

c_True = !1

c_Z = !1

c_bad = !2

c_unr = !1

cf(!1) <=> $true
cf(!2) <=> $false
cf(!3) <=> $false
cf(!4) <=> $true

f_listEq(!1,!1) = !3
f_listEq(!1,!2) = !3
f_listEq(!1,!3) = !1
f_listEq(!1,!4) = !1
f_listEq(!2,!1) = !1
f_listEq(!2,!2) = !1
f_listEq(!2,!3) = !3
f_listEq(!2,!4) = !1
f_listEq(!3,!1) = !1
f_listEq(!3,!2) = !3
f_listEq(!3,!3) = !3
f_listEq(!3,!4) = !1
f_listEq(!4,!1) = !1
f_listEq(!4,!2) = !2
f_listEq(!4,!3) = !1
f_listEq(!4,!4) = !2

min(!1) <=> $false
min(!2) <=> $true
min(!3) <=> $false
min(!4) <=> $true

p_0_Cons(!1) = !1
p_0_Cons(!2) = !2
p_0_Cons(!3) = !2
p_0_Cons(!4) = !4

p_0_S(!1) = !1
p_0_S(!2) = !1
p_0_S(!3) = !1
p_0_S(!4) = !4

p_1_Cons(!1) = !3
p_1_Cons(!2) = !1
p_1_Cons(!3) = !3
p_1_Cons(!4) = !1
+++ END MODEL

-}
module Models.ParadoxParser (parseParadoxModel) where

import Control.Arrow

import Data.Either
import Data.List
import Data.List.Split -- split


import Models.Model

parseParadoxModel :: String -> Model
parseParadoxModel xs = Model
    { dom_size   = d
    , functions  = f
    , predicates = p
    }
  where
    (d,r) = domainSize . cropModel . lines $ xs
    (f,p) = tabulateModel r

type RawModel = [String]

cropModel :: [String] -> RawModel
cropModel = drop 1
          . dropWhile (/= "+++ BEGIN MODEL")
          . takeWhile (/= "+++ END MODEL")

domainSize :: RawModel -> (DomSize,RawModel)
domainSize (x:xs) = (DomSize (read str),xs)
  where
    '%':' ':'d':'o':'m':'a':'i':'n':' ':'s':'i':'z':'e':' ':'i':'s':' ':str = x
domainSize _ = error "domainSize: empty model?"

tabulateModel :: RawModel -> ([Function],[Predicate])
tabulateModel = partitionEithers . map tabulate
              . filter (not . null) . splitWhen null

parseSymbol :: String -> Symbol
parseSymbol ('\'':xs)            = parseSymbol (reverse . drop 1 . reverse $ xs)
parseSymbol ('f':'_':xs)         = Fun xs
parseSymbol ('c':'_':xs)         = Constructor xs
parseSymbol ('p':'_':i:'_':xs)   = Projection (read [i]) xs
parseSymbol ('p':'_':i:j:'_':xs) = Projection (read [i,j]) xs
parseSymbol ('a':'_':xs)         = Skolem xs
parseSymbol ('p':'t':'r':'_':xs) = Pointer xs
parseSymbol "app"                = App
parseSymbol ('s':'K':xs)         = SkolemFunction xs
parseSymbol xs =
   error $ "parseSymbol, not a symbol: " ++ xs

parsePredicate :: String -> Pred
parsePredicate "min"        = Min
parsePredicate "$min"       = Min
parsePredicate "cf"         = CF
parsePredicate ('s':'P':xs) = SkolemPredicate xs
parsePredicate xs           = error $ "parsePredicate, not a predicate: " ++ xs

tabulate :: [String] -> Either Function Predicate
tabulate rs@(r:_)
    | " <=> " `isInfixOf` r = Right (tabPred parsed_rows)
    | otherwise             = Left (tabFunc parsed_rows)
  where
    parsed_rows = map parseRow rs
tabulate _ = error "tabulate: empty table"

elt :: String -> Elt
elt ('!':xs) = Elt (read xs)
elt xs       = error $ "elt, not an elt: " ++ xs

bool :: String -> Bool
bool "$true"  = True
bool "$false" = False
bool xs       = error $ "bool, not a bool: " ++ xs

type ParsedRow = (String,([Elt],String))

-- | Parses a function or a relational row
parseRow :: String -> ParsedRow
parseRow s = (name,(args,rhs))
  where
    (name,t:rest) = break (`elem` " (") s

    args
        | t == ' ' = []
        | t == '(' = map elt (splitWhen (== ',') (takeWhile (/= ')') rest))

    rhs = reverse . takeWhile (/= ' ') . reverse $ rest

-- | Tabulates a function
tabFunc :: [ParsedRow] -> Function
tabFunc (unzip -> ((s:_),xs)) = Function
    { function  = parseSymbol s
    , fun_table = map (second elt) xs
    }
tabFunc _ = error "tabFunc: no rows to parse"

-- | Tabulates a predicate
tabPred :: [ParsedRow] -> Predicate
tabPred (unzip -> ((p:_),xs)) = Predicate
    { predicate  = parsePredicate p
    , pred_table = map (second bool) xs
    }
tabPred _ = error "tabPred: no rows to parse"

-- | Tests `parseRow', should all be true
parseRowTests :: [Bool]
parseRowTests =
    [ parseRow "min(!4) <=> $true"  == ("min",([4],"$true"))
    , parseRow "p_0_Cons(!1) = !1"  == ("p_0_Cons",([1],"!1"))
    , parseRow "c_unr = !1"         == ("c_unr",([],"!1"))
    , parseRow "c_Cons(!4,!4) = !1" == ("c_Cons",([4,4],"!1"))
    , parseRow "'f_=='(!1,!1) = !1" == ("'f_=='",([1,1],"!1"))
    ]

