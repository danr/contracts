
* Quantification for a contract

We want to abstract this over whatever p we have here (maybe it should be CF)

    append_any_homomorphism p =
        (++) ::: CF :-> \xs
              -> CF :-> \ys
              -> CF :&: Pred (\zs -> any p zs <=> any p xs ++ any p ys)
      `Using` any_cf
      `Demanding` (p ::: CF --> CF)

* Partially applied functions/contracts

This is sort of the opposite to above:

    map_fromJust_ok = map fromJust ::: CF :&: Pred (all isJust) --> CF

* Arithmetic laws

Almost everywhere, you want to have the laws of arithmetic over Z.
(eg + assoc, commutative, <= reflexive, transitive) We need Integers!

* Some reasonable lemma system

As above, using associativity of ++ would be really handy but I don't
know how to fit that in the current framework
