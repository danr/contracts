% If you want CF -> CF too
% fof(x, hypothesis,
%           (! [Ct_d] :
%                ((min(Ct_d) & ~cf(Ct_d)) |
%                 (min(risers_hyp(le,Ct_d)) => cf(risers_hyp(le,Ct_d)))))).
fof(x,hypothesis,
          (! [Ct_e] :
               ((min(Ct_e) & ~cf(Ct_e)) |
                (min(Ct_e) &
                 min(full(Ct_e)) &
                 Ct_e != unr &
                 (full(Ct_e) = false | full(Ct_e) = bad)) |
                ((min(risers_hyp(le,Ct_e)) => cf(risers_hyp(le,Ct_e))) &
                 (min(risers_hyp(le,Ct_e)) =>
                  (min(full(risers_hyp(le,Ct_e))) &
                   (risers_hyp(le,Ct_e) = unr |
                    full(risers_hyp(le,Ct_e)) = unr |
                    full(risers_hyp(le,Ct_e)) = true))))))).
fof(x,axiom,(! [Ct_b,Ct_c] :
              ((min(Ct_b) & ~cf(Ct_b)) | (min(Ct_c) & ~cf(Ct_c)) |
                     (min(app(app(le,Ct_b),Ct_c)) =>
                      cf(app(app(le,Ct_b),Ct_c)))))).

% With this, 3.048s
fof(x,axiom,min(e) => cf(e)).
fof(x,axiom,min(e) => min(full(e))).
fof(x,axiom,min(e) => (e = unr | full(e) = unr | full(e) = true)).
fof(x,axiom,
     ( min(risers(le,e)) & ~cf(risers(le,e)))
   | ( min(risers(le,e))
     & min(full(risers(le,e)))
     & risers(le,e) != unr
     & (full(risers(le,e)) = false | full(risers(le,e)) = bad)
     )
   ).

% With this, 3.183s
%fof(x,axiom,
%%% If you want CF -> CF too
%%              (((min(d) => cf(d)) &
%%                min(risers(le,d)) &
%%                ~cf(risers(le,d)))) |
%              (((min(e) => cf(e)) &
%                (min(e) =>
%                 (min(full(e)) &
%                  (e = unr | full(e) = unr | full(e) = true))) &
%                ((min(risers(le,e)) &
%                  ~cf(risers(le,e))) |
%                 (min(risers(le,e)) &
%                  min(full(risers(le,e))) &
%                  risers(le,e) != unr &
%                  (full(risers(le,e)) = false |
%                   full(risers(le,e)) = bad)))))).

% But, interestingly, if I change axiom to negated_conjecture, vampire
% does not solve it. Baaaaaaaap

% GHC.Types.Bool
fof(x, axiom, false != true).
fof(x, axiom, false != bad).
fof(x, axiom, false != unr).
fof(x, axiom, true != bad).
fof(x, axiom, true != unr).

% []
fof(x, axiom,
    ! [X,Y] : ((min(cons(X,Y)) & min(X)) => p_0_cons(cons(X,Y)) = X)).
fof(x, axiom,
    ! [X,Y] : ((min(cons(X,Y)) & min(Y)) => p_1_cons(cons(X,Y)) = Y)).
fof(x, axiom,
    ! [X,Y] : ((min(nil) & min(cons(X,Y))) => nil != cons(X,Y))).
fof(x, axiom, min(nil) => nil != bad).
fof(x, axiom, min(nil) => nil != unr).
fof(x, axiom, ! [X,Y] : (min(cons(X,Y)) => cons(X,Y) != bad)).
fof(x, axiom, ! [X,Y] : (min(cons(X,Y)) => cons(X,Y) != unr)).

% risers_cbK = \ (@ a) (<= :: a -> a -> GHC.Types.Bool) (ds_daU :: [a]) ->
%   case ds_daU of _ {
%     [] -> GHC.Types.[] @ [a];
%     : x ds_daV ->
%       case ds_daV of _ {
%         [] ->
%           GHC.Types.:
%             @ [a] (GHC.Types.: @ a x (GHC.Types.[] @ a)) (GHC.Types.[] @ [a]);
%         : y xs ->
%           case risers_hyp_cbN @ a <= (GHC.Types.: @ a y xs) of _ {
%             [] -> lvl_risersBy_step_cbL @ a;
%             : s ss ->
%               case <= x y of _ {
%                 GHC.Types.False ->
%                   GHC.Types.:
%                     @ [a]
%                     (GHC.Types.: @ a x (GHC.Types.[] @ a))
%                     (GHC.Types.: @ [a] s ss);
%                 GHC.Types.True -> GHC.Types.: @ [a] (GHC.Types.: @ a x s) ss
%               }
%           }
%       }
%   }
% Dependencies: lvl_risersBy_step_cbL risers_hyp_cbN
fof(x, definition,
    ! [Le,Ds] : (min(risers(Le,Ds)) => min(Ds))).
fof(x, definition,
    ! [Le] :
        (min(risers(Le,bad)) => risers(Le,bad) = bad)).
fof(x, definition,
    ! [Le] :
        (min(risers(Le,nil)) => risers(Le,nil) = nil)).
fof(x, definition,
    ! [X,Ds,Le] : (min(risers(Le,cons(X,Ds))) => min(Ds))).
fof(x, definition,
    ! [X,Le] :
        (min(risers(Le,cons(X,bad))) =>
         risers(Le,cons(X,bad)) = bad)).
fof(x, definition,
    ! [X,Le] :
        (min(risers(Le,cons(X,nil))) =>
         risers(Le,cons(X,nil)) = cons(cons(X,nil),nil))).
fof(x, definition,
    ! [Y,Xs,X,Le] :
        (min(risers(Le,cons(X,cons(Y,Xs)))) =>
         min(risers_hyp(Le,cons(Y,Xs))))).
fof(x, definition,
    ! [Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          risers_hyp(Le,cons(Y,Xs)) = bad) =>
         risers(Le,cons(X,cons(Y,Xs))) = bad)).
fof(x, definition,
    ! [Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          risers_hyp(Le,cons(Y,Xs)) = nil) =>
         risers(Le,cons(X,cons(Y,Xs))) = lvl_risersby_step)).
fof(x, definition,
    ! [S,Ss,Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          risers_hyp(Le,cons(Y,Xs)) = cons(S,Ss)) =>
         min(app(app(Le,X),Y)))).
fof(x, definition,
    ! [S,Ss,Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          app(app(Le,X),Y) = bad &
          risers_hyp(Le,cons(Y,Xs)) = cons(S,Ss)) =>
         risers(Le,cons(X,cons(Y,Xs))) = bad)).
fof(x, definition,
    ! [S,Ss,Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          app(app(Le,X),Y) = false &
          risers_hyp(Le,cons(Y,Xs)) = cons(S,Ss)) =>
         risers(Le,cons(X,cons(Y,Xs))) = cons(cons(X,nil),
                                                      cons(S,Ss)))).
fof(x, definition,
    ! [S,Ss,Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          app(app(Le,X),Y) = true &
          risers_hyp(Le,cons(Y,Xs)) = cons(S,Ss)) =>
         risers(Le,cons(X,cons(Y,Xs))) = cons(cons(X,S),Ss))).
fof(x, definition,
    ! [S,Ss,Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          app(app(Le,X),Y) != bad &
          app(app(Le,X),Y) != false &
          app(app(Le,X),Y) != true &
          risers_hyp(Le,cons(Y,Xs)) = cons(S,Ss)) =>
         risers(Le,cons(X,cons(Y,Xs))) = unr)).
fof(x, definition,
    ! [Y,Xs,X,Le] :
        ((min(risers(Le,cons(X,cons(Y,Xs)))) &
          risers_hyp(Le,cons(Y,Xs)) != bad &
          risers_hyp(Le,cons(Y,Xs)) != nil &
          risers_hyp(Le,cons(Y,Xs)) != cons(p_0_cons(risers_hyp(Le,
                                                                    cons(Y,Xs))),
                                              p_1_cons(risers_hyp(Le,cons(Y,Xs))))) =>
         risers(Le,cons(X,cons(Y,Xs))) = unr)).
fof(x, definition,
    ! [X,Ds,Le] :
        ((min(risers(Le,cons(X,Ds))) &
          Ds != bad &
          Ds != nil &
          Ds != cons(p_0_cons(Ds),p_1_cons(Ds))) =>
         risers(Le,cons(X,Ds)) = unr)).
fof(x, definition,
    ! [Le,Ds] :
        ((min(risers(Le,Ds)) &
          Ds != bad &
          Ds != nil &
          Ds != cons(p_0_cons(Ds),p_1_cons(Ds))) =>
         risers(Le,Ds) = unr)).

% lvl_risersBy_step_cbL = \ (@ a) ->
%   Control.Exception.Base.patError
%     @ [[a]] "Risers.hs:13:11-40|pattern binding"
% Dependencies:
fof(x, definition,
    min(lvl_risersby_step) => lvl_risersby_step = bad).

% full = \ (@ t) (ds_daQ :: [t]) ->
%   case ds_daQ of _ {
%     [] -> GHC.Types.False;
%     : _ _ -> GHC.Types.True
%   }
% Dependencies:
fof(x, definition, ! [Ds] : (min(full(Ds)) => min(Ds))).
fof(x, definition, min(full(bad)) => full(bad) = bad).
fof(x, definition, min(full(nil)) => full(nil) = false).
fof(x, definition,
    ! [Ipv_0,Ipv] :
        (min(full(cons(Ipv_0,Ipv))) => full(cons(Ipv_0,Ipv)) = true)).
fof(x, definition,
    ! [Ds] :
        ((min(full(Ds)) &
          Ds != bad &
          Ds != nil &
          Ds != cons(p_0_cons(Ds),p_1_cons(Ds))) =>
         full(Ds) = unr)).

% App on min
fof(x, axiom, ! [F,X] : (min(app(F,X)) => min(F))).
fof(x, axiom, ! [X] : (min(app(unr,X)) => app(unr,X) = unr)).
fof(x, axiom, ! [X] : (min(app(bad,X)) => app(bad,X) = bad)).

% Axioms for BAD and UNR
fof(x, axiom, cf(unr)).
fof(x, axiom, ~cf(bad)).
fof(x, axiom, unr != bad).
fof(x, axiom, ! [X] : ((X != unr & cf(X)) => min(X))).

% CF GHC.Types.Bool
fof(x, axiom, cf(false)).
fof(x, axiom, cf(true)).

% CF []
fof(x, axiom, cf(nil)).
fof(x, axiom, ! [X,Y] : (cf(cons(X,Y)) => (cf(X) & cf(Y)))).
fof(x, axiom,
    ! [X,Y] :
        ((min(cons(X,Y)) & ~cf(cons(X,Y))) =>
         ((~cf(X) & min(X)) | (~cf(Y) & min(Y))))).
