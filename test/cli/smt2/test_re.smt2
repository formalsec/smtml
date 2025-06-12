(declare-const s String)
(assert (str.in_re s
(re.inter
(re.++ re.all (str.to_re "a")((_ re.loop 0 2) re.allchar) (str.to_re "b"))
(re.comp (re.++ re.all (str.to_re "b"))))))
(check-sat)