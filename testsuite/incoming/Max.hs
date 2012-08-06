module Max where

import Contracts
import Prelude (Bool(..),otherwise)

data Nat = Z | S Nat

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S Z

max_cf = max ::: CF --> CF --> CF

-- Optimised
-- max = \ (a :: Nat) (y :: Nat) ->
--   case a of _ {
--     Z -> y;
--     S _ ->
--       case y of _ {
--         Z -> a;
--         S _ ->
--           case a of _ {
--             Z ->
--               Control.Exception.Base.patError
--                 @ Nat "hs:(9,1)-(11,29)|function max";
--             S x ->
--               case y of _ {
--                 Z ->
--                   Control.Exception.Base.patError
--                     @ Nat "hs:(9,1)-(11,29)|function max";
--                 S y -> S (max x y)
--               }
--           }
--       }
--   }

-- Unoptimised
-- max = \ (a :: Nat) (y :: Nat) ->
--   case a of _ {
--     DEFAULT ->
--       case y of _ {
--         DEFAULT ->
--           case a of _ {
--             DEFAULT ->
--               Control.Exception.Base.patError
--                 @ Nat "hs:(9,1)-(11,29)|function max";
--             S x ->
--               case y of _ {
--                 DEFAULT ->
--                   Control.Exception.Base.patError
--                     @ Nat "hs:(9,1)-(11,29)|function max";
--                 S y -> S (max x y)
--               }
--           };
--         Z -> a
--       };
--     Z -> y
--   }
