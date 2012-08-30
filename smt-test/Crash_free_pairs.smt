
!1 = unreachable
!2 = crash
!3 = pair(!1,!2)

the activated patterns are unreachable, crash and pair.

(set-option :print-success false)
(declare-sort D)                 ;; our domain
(declare-fun crashfree (D) Bool) ;; the predicate of crash-freeness
(declare-fun pair (D D) D)       ;; pairs and its projections
(declare-fun fst (D) D)
(declare-fun snd (D) D)
(declare-fun crash () D)         ;; a program crash
(declare-fun unreachable () D)   ;; non-termination

;; Axioms for crash and unreachable,
(assert (= true (crashfree unreachable)))
(assert (= false (crashfree crash)))
(assert (distinct unreachable crash))

;; Pairs are not crashes
(assert (forall
         ((x D) (y D))
         (distinct (pair x y) crash)
         :pattern{ (pair x y) }))
;; Pairs are not unreachable
(assert (forall
         ((x D) (y D))
         (distinct (pair x y) unreachable)
         :pattern{ (pair x y) }))

;; First projection
(assert (forall
         ((x D) (y D)) (= (fst (pair x y)) x)
         :pattern{ (pair x y) }))
;; Second projection
(assert (forall
         ((x D) (y D))
         (= (snd (pair x y)) y)
         :pattern{ (pair x y) }))


;; A pair is crash free iff both its components are crash free.
(assert (forall
         ((x D) (y D))
         (= (crashfree (pair x y))
            (&& (crashfree x) (crashfree y)))
         :pattern{ (crashfree (pair x y)) }))

;; The pair (Crash,Unreachable) is not crashfree.
;; Should be countersatisfiable.
(assert (= false (crashfree (pair crash unreachable))))
(check-sat) ;; loops

;; in terms of crazy axioms
;;(assert (forall ((x D) (y D))
;;                (=> (= true (crashfree (pair x y)))
;;                    (and (= true (crashfree x)) (= true (crashfree y))))
;;                :pattern{ (crashfree (pair x y)) }
;;                ))
;;(assert (forall ((x D) (y D))
;;                (=> (= false (crashfree (pair x y)))
;;                    (or (= false (crashfree x)) (= false (crashfree y))))
;;                :pattern{ (crashfree (pair x y)) }
;;                ))

A model?

'c_()' = !3

pair(!1,!1) = !4
pair(!1,!2) = !4
pair(!1,!3) = !4
pair(!1,!4) = !3
pair(!2,!1) = !3
pair(!2,!2) = !2
pair(!2,!3) = !3
pair(!2,!4) = !3
pair(!3,!1) = !4
pair(!3,!2) = !3
pair(!3,!3) = !3
pair(!3,!4) = !3
pair(!4,!1) = !3
pair(!4,!2) = !3
pair(!4,!3) = !3
pair(!4,!4) = !3

fst(!1) = !1
fst(!2) = !1
fst(!3) = !4
fst(!4) = !1

snd(!1) = !1
snd(!2) = !1
snd(!3) = !3
snd(!4) = !1

c_BAD = !1
