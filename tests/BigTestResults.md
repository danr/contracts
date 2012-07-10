
Legend
------

    p: paradox
    x: equinox
    z: z3
    v: vampire
    e: eprover

lowercase:With min

Uppercase:Without min

SAT
===

    SAT AnyMorphism.big_sat_app_any_morphism_fail_step      p:2.52  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Loop.sat_id_loop_pred                               p:0.02  P:0.00  x:0.02  X:0.01  z:0.03  Z:0.01  v:----  V:----  e:0.01  E:0.01
    SAT Loop.sat_id_recursive_true                          p:0.03  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:0.01  E:0.01
    SAT PredLog.sat_concatMap_cf_missing_step               p:0.09  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT PredLog.sat_concatMap_retains_missing_step          p:0.61  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT PredLog.sat_flattenAnd_cf_missing_step              p:0.21  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT PredLog.sat_flattenAnd_retains_missing_step         p:0.46  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Recursion.big_sat_exp_accum_cf_broken_step          p:0.15  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Recursion.sat_exp_cf_broken_step                    p:0.10  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Recursion.sat_fac_cf_broken_step                    p:0.07  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Recursion.sat_mul_cf_broken_step                    p:0.04  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Recursion.sat_mult_cf_broken_step                   p:0.10  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Recursion.sat_qfac_cf_broken_step                   p:0.08  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Recursion.sat_rev_cf_broken_step                    p:0.08  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Risers.big_sat_risersBy_nonEmpty_broken2_step       p:7.07  P:----  x:9.14  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Risers.big_sat_risersBy_nonEmpty_broken_step        p:3.14  P:----  x:19.70 X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Risers.sat_risers_broken2_step                      p:0.35  P:----  x:5.65  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Risers.sat_risers_broken3_step                      p:0.32  P:----  x:10.26 X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Risers.sat_risers_broken_step                       p:0.88  P:----  x:11.31 X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Risers.sat_risers_missing_le_step                   p:1.01  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    SAT Shrink.big_sat_shrink_lazy_step                     p:----  P:----  x:7.16  X:----  z:----  Z:----  v:----  V:----  e:----  E:----

UNS
===

    UNS Ack.big_unsat_ack_cf_step                           p:----  P:----  x:1.61  X:----  z:0.04  Z:0.02  v:0.47  V:0.14  e:0.08  E:----
    UNS Ack.unsat_plus_cf_step                              p:----  P:----  x:0.18  X:----  z:0.04  Z:0.00  v:0.06  V:2.37  e:0.02  E:0.10
    UNS AnyMorphism.big_unsat_app_any_morphism_step         p:----  P:----  x:10.40 X:----  z:0.07  Z:0.04  v:----  V:----  e:----  E:----
    UNS AnyMorphism.unsat_any_cf_step                       p:----  P:----  x:0.61  X:----  z:0.04  Z:0.01  v:0.43  V:2.21  e:0.06  E:0.05
    UNS AnyMorphism.unsat_app_cf_step                       p:----  P:----  x:0.51  X:----  z:0.04  Z:0.03  v:1.77  V:2.31  e:0.03  E:0.46
    UNS Filter.big_unsat_filter_all_step                    p:----  P:----  x:3.24  X:----  z:0.05  Z:0.04  v:----  V:----  e:----  E:----
    UNS Filter.unsat_all_cf_step                            p:----  P:----  x:0.60  X:----  z:0.03  Z:0.00  v:0.31  V:1.81  e:0.06  E:0.05
    UNS Infinite.unsat_iterate_cf_step                      p:----  P:----  x:0.42  X:5.82  z:0.03  Z:0.00  v:0.18  V:0.00  e:0.03  E:0.01
    UNS Infinite.unsat_iterTree_cf_step                     p:----  P:----  x:1.85  X:----  z:0.03  Z:0.00  v:2.90  V:0.01  e:0.60  E:0.01
    UNS Infinite.unsat_lefts_cf_step                        p:----  P:----  x:0.40  X:5.10  z:0.03  Z:0.00  v:0.23  V:2.72  e:0.02  E:0.02
    UNS Infinite.unsat_repeat_cf_step                       p:----  P:----  x:0.10  X:0.05  z:0.03  Z:0.00  v:0.01  V:0.00  e:0.02  E:0.01
    UNS Loop.unsat_id_four_rec_true                         p:----  P:----  x:0.34  X:0.06  z:0.03  Z:0.03  v:0.06  V:0.01  e:0.02  E:0.00
    UNS Loop.unsat_loop_cf_step                             p:0.01  P:0.00  x:0.00  X:0.00  z:0.00  Z:0.00  v:0.00  V:0.00  e:0.01  E:0.01
    UNS Loop.unsat_loop_isnt_zero_step                      p:----  P:----  x:0.02  X:0.01  z:0.02  Z:0.00  v:0.03  V:0.01  e:0.00  E:0.01
    UNS Loop.unsat_loop_zero_step                           p:----  P:----  x:0.01  X:0.00  z:0.04  Z:0.00  v:0.01  V:0.01  e:0.01  E:0.00
    UNS Loop.unsat_recursive_true_and_cf_step               p:----  P:----  x:0.17  X:0.12  z:0.03  Z:0.01  v:0.60  V:0.22  e:0.02  E:0.01
    UNS Loop.unsat_recursive_true_cf_step                   p:----  P:----  x:0.10  X:0.06  z:0.03  Z:0.00  v:0.01  V:0.01  e:0.01  E:0.00
    UNS PredLog.big_unsat_invariant_cf_step                 p:----  P:----  x:23.37 X:----  z:0.08  Z:----  v:----  V:----  e:----  E:----
    UNS PredLog.big_unsat_map_retains_invariant_step        p:----  P:----  x:22.82 X:----  z:0.17  Z:----  v:----  V:----  e:----  E:----
    UNS PredLog.big_unsat_neg_retains_invariant_step        p:----  P:----  x:----  X:----  z:0.83  Z:----  v:----  V:----  e:----  E:----
    UNS PredLog.unsat_all_cf_step                           p:----  P:----  x:0.58  X:----  z:0.03  Z:0.00  v:0.31  V:2.50  e:0.06  E:0.05
    UNS PredLog.unsat_append_cf_step                        p:----  P:----  x:0.32  X:----  z:0.04  Z:0.03  v:0.28  V:2.61  e:0.05  E:0.40
    UNS PredLog.unsat_append_retains_invariant_step         p:----  P:----  x:----  X:----  z:0.08  Z:----  v:2.70  V:----  e:----  E:----
    UNS PredLog.unsat_concatMap_cf_step                     p:----  P:----  x:0.87  X:----  z:0.04  Z:0.04  v:0.38  V:4.65  e:0.02  E:----
    UNS PredLog.unsat_concatMap_retains_invariant_step      p:FAIL  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    UNS PredLog.unsat_flattenAnd_cf_step                    p:----  P:----  x:3.29  X:----  z:0.04  Z:0.04  v:0.17  V:0.06  e:0.03  E:0.09
    UNS PredLog.unsat_flattenAnd_retains_invariant_step     p:----  P:----  x:13.43 X:----  z:0.14  Z:----  v:4.07  V:----  e:----  E:----
    UNS Recursion.big_unsat_exp_accum_cf_step               p:----  P:----  x:0.54  X:----  z:0.06  Z:0.04  v:2.27  V:3.44  e:0.01  E:0.13
    UNS Recursion.unsat_and_cf                              p:----  P:----  x:0.03  X:0.01  z:0.03  Z:0.00  v:0.01  V:0.01  e:0.01  E:0.00
    UNS Recursion.unsat_append_cf_step                      p:----  P:----  x:0.29  X:----  z:0.03  Z:0.03  v:0.26  V:2.50  e:0.06  E:0.40
    UNS Recursion.unsat_double_cf_step                      p:----  P:----  x:0.13  X:0.24  z:0.04  Z:0.03  v:0.04  V:1.68  e:0.02  E:0.01
    UNS Recursion.unsat_even_cf_step                        p:----  P:----  x:0.22  X:19.08 z:0.03  Z:0.00  v:0.06  V:0.00  e:0.02  E:0.02
    UNS Recursion.unsat_exp_cf_step                         p:----  P:----  x:0.32  X:----  z:0.04  Z:0.03  v:0.14  V:2.49  e:0.67  E:----
    UNS Recursion.unsat_fac_cf_step                         p:----  P:----  x:0.23  X:7.17  z:0.04  Z:0.03  v:0.04  V:3.04  e:0.05  E:33.02
    UNS Recursion.unsat_half_cf_step                        p:----  P:----  x:0.21  X:42.50 z:0.04  Z:0.00  v:0.09  V:2.46  e:0.04  E:0.11
    UNS Recursion.unsat_ind_cf_step                         p:----  P:----  x:0.09  X:0.10  z:0.03  Z:0.00  v:0.00  V:0.31  e:0.01  E:0.01
    UNS Recursion.unsat_length_cf_step                      p:----  P:----  x:0.20  X:0.92  z:0.02  Z:0.00  v:0.03  V:2.86  e:0.01  E:0.01
    UNS Recursion.unsat_mul_cf_step                         p:----  P:----  x:0.24  X:6.64  z:0.04  Z:0.04  v:0.13  V:1.93  e:4.31  E:----
    UNS Recursion.unsat_mult_cf_step                        p:----  P:----  x:0.40  X:----  z:0.06  Z:0.04  v:2.26  V:1.78  e:0.02  E:----
    UNS Recursion.unsat_not_cf                              p:----  P:----  x:0.02  X:0.00  z:0.03  Z:0.00  v:0.01  V:0.00  e:0.00  E:0.00
    UNS Recursion.unsat_or_cf                               p:----  P:----  x:0.03  X:0.02  z:0.04  Z:0.00  v:0.02  V:0.01  e:0.01  E:0.00
    UNS Recursion.unsat_plus_cf_step                        p:----  P:----  x:0.20  X:----  z:0.03  Z:0.00  v:0.07  V:2.52  e:0.02  E:0.10
    UNS Recursion.unsat_qfac_cf_step                        p:----  P:----  x:0.30  X:----  z:0.03  Z:0.03  v:0.11  V:2.32  e:0.06  E:----
    UNS Recursion.unsat_rev_cf_step                         p:----  P:----  x:1.35  X:14.56 z:0.03  Z:0.02  v:0.33  V:21.28 e:0.05  E:----
    UNS RecursiveTrue.unsat_recursive_true_and_cf_step      p:----  P:----  x:0.17  X:0.10  z:0.04  Z:0.00  v:0.53  V:0.21  e:0.02  E:0.00
    UNS Risers.big_unsat_risersBy_nonEmpty_step             p:----  P:----  x:26.18 X:----  z:----  Z:----  v:7.80  V:0.82  e:----  E:----
    UNS Risers.unsat_le_step                                p:----  P:----  x:0.88  X:----  z:0.04  Z:0.00  v:0.26  V:0.23  e:0.05  E:----
    UNS Risers.unsat_risersBy_step                          p:----  P:----  x:37.02 X:----  z:----  Z:----  v:----  V:1.10  e:----  E:----
    UNS Risers.unsat_risers_step                            p:----  P:----  x:----  X:----  z:----  Z:----  v:----  V:----  e:----  E:----
    UNS Shrink.big_unsat_shrink_step                        p:----  P:----  x:5.64  X:----  z:0.05  Z:0.05  v:2.04  V:----  e:----  E:----
    UNS Shrink.unsat_fromJust_contr                         p:----  P:----  x:0.10  X:0.07  z:0.03  Z:0.00  v:0.29  V:0.00  e:0.01  E:0.01
    UNS WithMany.big_unsat_wmc_step                         p:----  P:----  x:21.51 X:----  z:0.05  Z:0.01  v:3.96  V:----  e:----  E:----
    UNS Xor.unsat_and_cf                                    p:----  P:----  x:0.03  X:0.02  z:0.03  Z:0.00  v:0.01  V:0.01  e:0.01  E:0.00
    UNS Xor.unsat_ind_contr_cf_step                         p:----  P:----  x:0.25  X:1.05  z:0.03  Z:0.00  v:0.26  V:3.80  e:0.06  E:0.05
    UNS Xor.unsat_ind_contr_retain_step                     p:----  P:----  x:8.26  X:----  z:0.06  Z:----  v:4.18  V:3.78  e:----  E:----
    UNS Xor.unsat_invariant_cf_step                         p:----  P:----  x:0.29  X:----  z:0.04  Z:0.01  v:0.51  V:2.08  e:0.06  E:0.02

Simple tests
============

Simple SAT
----------

    SAT Bad.sat_w2                                          p:0.00  P:0.00  x:0.01  X:0.00  z:0.00  Z:0.00  v:----  V:----  e:0.00  E:0.00
    SAT Bad.sat_w                                           p:0.01  P:0.00  x:0.00  X:0.00  z:0.00  Z:0.00  v:----  V:----  e:0.00  E:0.00
    SAT Simple.sat_bad_cf_id                                p:0.00  P:0.00  x:0.01  X:0.00  z:0.04  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_bad_cf_not                               p:0.01  P:0.01  x:0.01  X:0.01  z:----  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_bad_cf                                   p:0.00  P:0.00  x:0.00  X:0.01  z:0.00  Z:0.00  v:----  V:----  e:0.01  E:0.01
    SAT Simple.sat_bad_id                                   p:0.01  P:0.01  x:0.01  X:0.00  z:0.04  Z:0.00  v:----  V:----  e:0.00  E:0.01
    SAT Simple.sat_bad_not                                  p:0.02  P:0.01  x:0.01  X:0.02  z:----  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_boom_bad                                 p:0.00  P:0.00  x:0.01  X:0.00  z:0.00  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_boom_boom                                p:0.00  P:0.01  x:0.01  X:0.00  z:0.01  Z:0.00  v:----  V:----  e:0.00  E:0.00
    SAT Simple.sat_boom_unboom                              p:0.00  P:0.01  x:0.01  X:0.00  z:0.00  Z:0.00  v:----  V:----  e:0.01  E:0.01
    SAT Simple.sat_const_cf_res                             p:0.02  P:0.01  x:0.02  X:0.01  z:0.03  Z:0.00  v:----  V:----  e:0.00  E:0.01
    SAT Simple.sat_false_cf_id                              p:0.01  P:0.00  x:0.01  X:0.00  z:0.04  Z:0.00  v:----  V:----  e:0.00  E:0.00
    SAT Simple.sat_false_id                                 p:0.01  P:0.00  x:0.00  X:0.01  z:0.04  Z:0.00  v:----  V:----  e:0.00  E:0.00
    SAT Simple.sat_id_cf_const                              p:0.02  P:0.01  x:0.02  X:0.01  z:0.04  Z:0.01  v:----  V:----  e:0.01  E:0.01
    SAT Simple.sat_id_cf_id                                 p:0.01  P:0.00  x:0.01  X:0.00  z:0.03  Z:0.00  v:----  V:----  e:0.00  E:0.00
    SAT Simple.sat_id_cf_not                                p:0.03  P:0.01  x:0.03  X:0.02  z:0.04  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_id_const                                 p:0.02  P:0.02  x:0.01  X:0.00  z:0.02  Z:0.01  v:----  V:----  e:0.00  E:0.00
    SAT Simple.sat_id_id                                    p:0.01  P:0.00  x:0.02  X:0.01  z:0.04  Z:0.00  v:----  V:----  e:0.01  E:0.01
    SAT Simple.sat_id_not                                   p:0.02  P:0.01  x:0.02  X:0.02  z:0.04  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_not_cf_id                                p:0.01  P:0.01  x:0.04  X:0.02  z:0.04  Z:0.00  v:----  V:----  e:0.00  E:0.01
    SAT Simple.sat_not_cf_not                               p:0.01  P:0.02  x:0.01  X:0.02  z:0.04  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_not_id                                   p:0.01  P:0.02  x:0.02  X:0.02  z:0.03  Z:0.01  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_not_not                                  p:0.01  P:0.01  x:0.04  X:0.01  z:0.03  Z:0.00  v:----  V:----  e:0.01  E:0.00
    SAT Simple.sat_true_cf_not                              p:0.01  P:0.01  x:0.01  X:0.00  z:0.04  Z:0.00  v:----  V:----  e:0.00  E:0.01
    SAT Simple.sat_true_not                                 p:0.01  P:0.01  x:0.01  X:0.00  z:0.03  Z:0.00  v:----  V:----  e:0.00  E:0.00
    SAT Head.sat_head_broken_1                              p:0.04  P:----  x:0.12  X:----  z:0.06  Z:----  v:----  V:----  e:0.06  E:0.01
    SAT Head.sat_head_broken_2                              p:0.37  P:----  x:0.44  X:----  z:----  Z:----  v:----  V:----  e:----  E:0.01

Simple UNS
----------

    UNS Head.unsat_head_contract                            p:----  P:----  x:0.26  X:18.70 z:0.03  Z:0.00  v:2.16  V:0.00  e:0.04  E:0.01
    UNS Simple.unsat_bad_unjuggle_id                        p:----  P:----  x:0.02  X:0.01  z:0.04  Z:0.00  v:0.05  V:0.00  e:0.00  E:0.00
    UNS Simple.unsat_bad_unjuggle_not                       p:----  P:----  x:0.05  X:0.00  z:0.03  Z:0.00  v:0.14  V:0.00  e:0.02  E:0.00
    UNS Simple.unsat_const_boom                             p:----  P:0.05  x:0.01  X:0.01  z:0.04  Z:0.00  v:0.00  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_const_cf_eq                            p:----  P:----  x:0.05  X:0.02  z:0.04  Z:0.00  v:0.24  V:0.00  e:0.01  E:0.01
    UNS Simple.unsat_const_cf                               p:----  P:0.01  x:0.01  X:0.00  z:0.03  Z:0.00  v:0.00  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_false_cf_not                           p:0.00  P:0.01  x:0.02  X:0.00  z:0.03  Z:0.00  v:0.00  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_false_cf                               p:0.00  P:0.00  x:0.00  X:0.00  z:0.00  Z:0.00  v:0.00  V:0.00  e:0.00  E:0.01
    UNS Simple.unsat_false_not                              p:0.00  P:0.00  x:0.00  X:0.00  z:0.03  Z:0.00  v:0.01  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_id_cf                                  p:----  P:0.01  x:0.00  X:0.00  z:0.04  Z:0.00  v:0.00  V:0.00  e:0.00  E:0.00
    UNS Simple.unsat_id_eq                                  p:----  P:----  x:0.06  X:0.02  z:0.04  Z:0.00  v:0.33  V:0.01  e:0.02  E:0.01
    UNS Simple.unsat_juggle_id_cf                           p:----  P:0.01  x:0.00  X:0.00  z:0.03  Z:0.00  v:0.00  V:0.00  e:0.00  E:0.00
    UNS Simple.unsat_juggle_id                              p:----  P:0.00  x:0.01  X:0.01  z:0.03  Z:0.00  v:0.00  V:0.00  e:0.00  E:0.00
    UNS Simple.unsat_juggle_not_cf                          p:----  P:0.00  x:0.03  X:0.01  z:0.03  Z:0.00  v:0.26  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_juggle_not                             p:----  P:0.00  x:0.02  X:0.02  z:0.02  Z:0.00  v:0.10  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_not_cf                                 p:----  P:----  x:0.02  X:0.01  z:0.02  Z:0.00  v:0.01  V:0.00  e:0.01  E:0.01
    UNS Simple.unsat_not_uneq                               p:----  P:----  x:0.11  X:0.03  z:0.04  Z:0.00  v:2.24  V:0.00  e:0.10  E:0.01
    UNS Simple.unsat_true_cf_id                             p:----  P:0.01  x:0.01  X:0.00  z:0.03  Z:0.00  v:0.00  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_true_cf                                p:0.00  P:0.00  x:0.01  X:0.00  z:0.00  Z:0.00  v:0.00  V:0.00  e:0.00  E:0.00
    UNS Simple.unsat_true_id                                p:----  P:0.00  x:0.01  X:0.00  z:0.03  Z:0.00  v:0.00  V:0.00  e:0.00  E:0.00
    UNS Simple.unsat_unjuggle_cf_id                         p:----  P:----  x:0.01  X:0.02  z:0.03  Z:0.00  v:0.05  V:0.00  e:0.01  E:0.00
    UNS Simple.unsat_unjuggle_cf_not                        p:----  P:----  x:0.02  X:0.01  z:0.03  Z:0.00  v:0.92  V:0.01  e:0.01  E:0.00
    UNS Simple.unsat_unjuggle_id                            p:----  P:----  x:0.03  X:0.01  z:0.03  Z:0.00  v:0.04  V:0.00  e:0.00  E:0.00
    UNS Simple.unsat_unjuggle_not                           p:----  P:----  x:0.03  X:0.00  z:0.03  Z:0.00  v:1.43  V:0.00  e:0.01  E:0.01

