# Ite
- ite(e1, true, e3) = e1 or e3
- ite(e1, e2, true) = !e1 or e2
- ite(e1, e1, e2) = e1 or e2
- ite(e1, e2, e1) = e1 and e2

# != with < ...
- (bool.and (bool.not (bool.eq 10. y)) (real.lt 10. y)) 
- x != y and x < y      = x < y  
- x != y and x > y      = x > y  
- x != y and x <= y     = x < y
- x != y and x >= y     = x > y

- x = y and x < y       = x <= y
- x = y and x > y       = x >= y
- x = y and x <= y       = x <= y
- x = y and x >= y       = x >= y

# Negation of relational operators
- !(x = y)             =  x != y
- !(x != y)            =  x = y
- !(x < y)             =  x >= y
- !(x > y)             =  x <= y
- !(x >= y)             =  x < y
- !(x >= y)             =  x < y

# s != v1 and s = v2 -> symbol com values
- s != v1 and s = v2    = false  if v1 = v2
- s != v1 and s = v2    = s = v2
- and change operator order
  
# igualdade
- Symbol s = Val App     = false
- Symbol s = Val v       = false if (type s) != (type v)
- and change operator order


# eq com ite e val
- Val v = Ite(e1, e2, e3)   = Ite(e1, Val v = e2, Val v = e3)
- (bool.eq (bool.ite (bool.not (bool.eq 100. y)) "symbol" "object") "object")
- ite(100 != y, "symbol", "object") = "object" ------> 
	ite(100 != y, "symbol" = "object", "object" = "object")
	ite(100 != y, false, true)
	100 = y

# Or
- e or e                     = e
- false or e                 = e
- e or false                 = e
- e1 or (e1 and e4)          = e1
- e1 or (e3 and e1)          = e1
- e1 or (e3 or e4)           = e1 or Lor(e3;e4)
- (e1 or e2) or e3           = e1 or Lor(e2;e3)
- e or Lor(es)               = Lor(e;es)
- e in S1 or e in S2         = e in (S1 union S2)
- e notin S1 or e notin S2   = e notin (S1 inter S2)
- e in S1 and e notin S2     = e in S2-S1            if S1 is a subset of S2
- e notin S1 and e in S2     = e in S1-S2            if S2 is a subset of S1

# And 
- e and e                    = e
- e1 and (e1 or e4)          = e1
- e1 and (e3 or e1)          = e1
- (e1 or e3) and e3          = e3
- (e3 or e2) and e3          = e3
- e1 and Lor(es)             = e1 if e1 in es
- Lor(es) and e2             = e2 if e2 in es
- e1 and (e1 and e4)         = e1 and e4
- e1 and (e3 and e1)         = e1 and e3
- e1 and (e3 and e4)         = e1 and Land(e3;e4)
- (e1 and e2) and e3         = e1 and Land(e2;e3)
- e and Land(es)             = Land(e;es)
- Land(es) and e             = Land(e;es)
- e notin S1 and e notin S2  = e notin (S1 union S2)
- e in S1 and e in S2        = e in (S1 inter S2)
- e in S1 and e notin S2     = e in S1              if (S1 inter S2) = {}
  

# Eq between App and Relop
- Val App = Relop ...        = false
- Relop ... = Val App        = false