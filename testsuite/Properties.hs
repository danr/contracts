module Properties where

import Contracts

reflexive (~~) = All (\x -> x ::: CF :=> x ~~ x ::: CF :&: Pred id)

symmetric (~~) = All $ \x -> All $ \y ->
    x ::: CF :=> y ::: CF :=>
    x ~~ y ::: CF :&: Pred id :=>
    y ~~ x ::: CF :&: Pred id

transitive (~~) = (All $ \x -> All $ \y -> All $ \z ->
    x ::: CF :=> y ::: CF :=> z ::: CF :=>
    x ~~ y ::: CF :&: Pred id :=>
    y ~~ z ::: CF :&: Pred id :=>
    x ~~ z ::: CF :&: Pred id)

(*) `idempotentOver` (===)  = All $ \x ->
    x ::: CF :=> x * x ::: CF :&: Pred (=== x)

(*) `commutativeOver` (===) =
    (*) ::: CF :-> \x -> CF :-> \y -> CF :&: Pred (=== (y * x))

(*) `associativeOver` (===) = All $ \z -> z ::: CF :=>
    (*) ::: CF :-> \x -> CF :-> \y -> CF :&: Pred (\r -> (r * z) === (x * (y * z)))
