{-

    Pretty printing of paradox models

    For each domain element we calculate the elements of its
    equivalence class.

    With type information we could print a "right" representation from
    the equivalence class. For now, we just list them, separated by
    slashes.

-}
module Main where

import Control.Arrow
import Data.List
import Data.List.Split -- split


{-
    Typical input

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

main :: IO ()
main = interact paramod

type RawModel = [String]

paramod :: String -> String
paramod = unlines
        . map show
        . sort
        . second tabulateModel
        . domainSize
        . cropmodel
        . lines

cropmodel :: [String] -> RawModel
cropmodel = drop 1
          . dropWhile (/= "+++ BEGIN MODEL")
          . takeWhile (/= "+++ END MODEL")

domainSize :: RawModel -> (Int,RawModel)
domainSize (x:xs) = (read str,xs)
  where
    '%':' ':'d':'o':'m':'a':'i':'n':' ':'s':'i':'z':'e':' ':'i':'s':' ':str = x

data Symbol
    = Function String
    | Constructor String
    | Skolem String
    | Projection Int String
    | App
  deriving (Show,Eq,Ord)

data Predicate = Min | CF
  deriving (Show,Eq,Ord)

tabulateModel :: RawModel -> [Table]
tabulateModel = map tabulate . splitWhen null

data Table
    = Func Symbol [([Int],Int)]
    | Pred Predicate [(Int,Bool)]
  deriving (Show,Eq,Ord)

parseSymbol :: String -> Symbol
parseSymbol ('\'':xs)            = parseSymbol (reverse . drop 1 . reverse $ xs)
parseSymbol ('f':'_':xs)         = Function xs
parseSymbol ('c':'_':xs)         = Constructor xs
parseSymbol ('p':'_':i:'_':xs)   = Projection (read [i]) xs
parseSymbol ('p':'_':i:j:'_':xs) = Projection (read [i,j]) xs
parseSymbol ('a':'_':xs)         = Skolem xs
parseSymbol "app"                = App
parseSymbol xs                   = error $ "parseSymbol, not a symbol: " ++ xs

parsePredicate :: String -> Predicate
parsePredicate "min"  = Min
parsePredicate "$min" = Min
parsePredicate "cf"   = CF
parsePredicate xs     = error $ "parsePredicate, not a predicate: " ++ xs

tabulate :: [String] -> Table
tabulate rs@(r:_)
    | " <=> " `isInfixOf` r = tabPred parsed_rows
    | otherwise             = tabFunc parsed_rows
  where
    parsed_rows = map parseRow rs

elt :: String -> Int
elt ('!':xs) = read xs

bool :: String -> Bool
bool "$true"  = True
bool "$false" = False
bool xs       = error $ "bool, not a bool: " ++ xs

type ParsedRow = (String,([Int],String))

-- | Parses a function or a relational row
parseRow :: String -> ParsedRow
parseRow s = (name,(args,rhs))
  where
    (name,t:rest) = break (`elem` " (") s

    args
        | t == ' ' = []
        | t == '(' = map elt (splitWhen (== ',') (takeWhile (/= ')') rest))

    rhs = reverse . takeWhile (/= ' ') . reverse $ rest

-- | Tests `parseRow', should all be true
parseRowTests :: [Bool]
parseRowTests =
    [ parseRow "min(!4) <=> $true"  == ("min",([4],"$true"))
    , parseRow "p_0_Cons(!1) = !1"  == ("p_0_Cons",([1],"!1"))
    , parseRow "c_unr = !1"         == ("c_unr",([],"!1"))
    , parseRow "c_Cons(!4,!4) = !1" == ("c_Cons",([4,4],"!1"))
    , parseRow "'f_=='(!1,!1) = !1" == ("'f_=='",([1,1],"!1"))
    ]

-- | Tabulates a function
tabFunc :: [ParsedRow] -> Table
tabFunc = uncurry Func
        . (parseSymbol . fst . head &&& map (second elt . snd))

-- | Tabulates a predicate
tabPred :: [ParsedRow] -> Table
tabPred = uncurry Pred
        . (parsePredicate . fst . head &&& map ((head *** bool) . snd))
