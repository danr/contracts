{-

    Unfinished, but here is an example output of a z3 model:

c_False -> $i!val!0
c_BAD -> $i!val!2
c_UNR -> $i!val!3
a_a -> $i!val!4
f_loop_pred -> $i!val!5
c_True -> $i!val!1
cf!1 -> {
  $i!val!2 -> false
  else -> true
}
app -> {
  $i!val!0
}
min -> {
  true
}
cf -> {
  (cf!1 (k!0 #0))
}
k!0 -> {
  $i!val!3 -> $i!val!3
  $i!val!2 -> $i!val!2
  $i!val!4 -> $i!val!4
  $i!val!1 -> $i!val!1
  else -> $i!val!0
}
f_id -> {
  $i!val!4 -> $i!val!4
  else -> #0
}

    Some notes:

        there is a boolean sort which predicates are defined
        in terms of

        I am not sure what k!0 and #0 are though :)
        Seems to be the 0th argument.

Here is a bigger example:

(paramod) $ z3 -tptp Risers.broken_risers_2_step.tptp.z3
SZS status Satisfiable for Risers.broken_risers_2_step.tptp.z3
c_Z -> $i!val!3
c_False -> $i!val!0
c_BAD -> $i!val!7
c_UNR -> $i!val!2
a_g -> $i!val!8
c_True -> $i!val!1
c_Nil -> $i!val!10
c_S -> {
  $i!val!5
}
f_risers_hyp -> {
  $i!val!10
}
'f_<=' -> {
  ('f_<='!96 (k!93 #0) (k!93 #1))
}
p_1_Cons -> {
  (p_1_Cons!97 (k!93 #0))
}
p_0_S -> {
  $i!val!5
}
p_0_Cons!94 -> {
  $i!val!10 -> $i!val!11
  $i!val!2 -> $i!val!24
  $i!val!32 -> $i!val!29
  else -> $i!val!2
}
cf -> {
  (cf!99 (k!93 #0))
}
'f_<='!96 -> {
  $i!val!3 $i!val!3 -> $i!val!6
  else -> $i!val!5
}
c_Cons!98 -> {
  $i!val!2 $i!val!9 -> $i!val!8
  $i!val!2 $i!val!2 -> $i!val!15
  $i!val!2 $i!val!7 -> $i!val!16
  $i!val!2 $i!val!14 -> $i!val!17
  $i!val!7 $i!val!10 -> $i!val!19
  $i!val!19 $i!val!10 -> $i!val!21
  $i!val!5 $i!val!2 -> $i!val!22
  $i!val!24 $i!val!25 -> $i!val!26
  $i!val!14 $i!val!2 -> $i!val!27
  $i!val!29 $i!val!30 -> $i!val!31
  $i!val!2 $i!val!32 -> $i!val!9
  else -> $i!val!13
}
cf!99 -> {
  $i!val!2 -> true
  $i!val!0 -> true
  $i!val!1 -> true
  $i!val!3 -> true
  $i!val!8 -> true
  $i!val!10 -> true
  $i!val!9 -> true
  $i!val!32 -> true
  else -> false
}
min!100 -> {
  $i!val!0 -> true
  $i!val!1 -> true
  $i!val!3 -> true
  $i!val!8 -> true
  $i!val!7 -> true
  $i!val!9 -> true
  $i!val!32 -> true
  $i!val!10 -> true
  $i!val!12 -> true
  $i!val!24 -> true
  $i!val!29 -> true
  else -> false
}
p_0_Cons -> {
  (p_0_Cons!94 (k!93 #0))
}
f_risers_concl -> {
  (f_risers_concl!95 (k!93 #0))
}
k!93 -> {
  $i!val!35 -> $i!val!35
  $i!val!26 -> $i!val!26
  $i!val!21 -> $i!val!21
  $i!val!33 -> $i!val!33
  $i!val!17 -> $i!val!17
  $i!val!11 -> $i!val!11
  $i!val!32 -> $i!val!32
  $i!val!8 -> $i!val!8
  $i!val!16 -> $i!val!16
  $i!val!19 -> $i!val!19
  $i!val!14 -> $i!val!14
  $i!val!34 -> $i!val!34
  $i!val!9 -> $i!val!9
  $i!val!12 -> $i!val!12
  $i!val!29 -> $i!val!29
  $i!val!23 -> $i!val!23
  $i!val!7 -> $i!val!7
  $i!val!20 -> $i!val!20
  $i!val!2 -> $i!val!2
  $i!val!24 -> $i!val!24
  $i!val!22 -> $i!val!22
  $i!val!38 -> $i!val!38
  $i!val!40 -> $i!val!40
  $i!val!28 -> $i!val!28
  $i!val!39 -> $i!val!39
  $i!val!13 -> $i!val!13
  $i!val!36 -> $i!val!36
  $i!val!18 -> $i!val!18
  $i!val!10 -> $i!val!10
  $i!val!3 -> $i!val!3
  $i!val!30 -> $i!val!30
  $i!val!1 -> $i!val!1
  $i!val!4 -> $i!val!4
  $i!val!25 -> $i!val!25
  $i!val!15 -> $i!val!15
  $i!val!27 -> $i!val!27
  $i!val!37 -> $i!val!37
  $i!val!31 -> $i!val!31
  $i!val!0 -> $i!val!0
  $i!val!6 -> $i!val!6
  else -> $i!val!5
}
f_risers_concl!95 -> {
  $i!val!8 -> $i!val!7
  $i!val!9 -> $i!val!14
  $i!val!17 -> $i!val!18
  $i!val!19 -> $i!val!20
  $i!val!22 -> $i!val!23
  $i!val!27 -> $i!val!28
  $i!val!16 -> $i!val!33
  $i!val!13 -> $i!val!34
  $i!val!10 -> $i!val!35
  $i!val!15 -> $i!val!36
  $i!val!26 -> $i!val!37
  $i!val!2 -> $i!val!38
  $i!val!31 -> $i!val!39
  $i!val!32 -> $i!val!40
  else -> $i!val!4
}
min -> {
  (min!100 (k!93 #0))
}
c_Cons -> {
  (c_Cons!98 (k!93 #0) (k!93 #1))
}
p_1_Cons!97 -> {
  $i!val!10 -> $i!val!12
  $i!val!2 -> $i!val!25
  $i!val!32 -> $i!val!30
  $i!val!9 -> $i!val!32
  else -> $i!val!9
}


-}
