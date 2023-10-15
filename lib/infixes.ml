open Expression

module IntInfix = struct 
  let (+) e1 e2  = Binop (Int Add, e1, e2) 
  let (-) e1 e2 = Binop (Int Sub, e1, e2)
  let ( * ) e1 e2 = Binop (Int Mul, e1, e2)
  let ( / ) e1 e2 = Binop (Int Div, e1, e2)
  let (<) e1 e2 = Relop (Int Lt, e1, e2)
  let (>) e1 e2 = Relop (Int Gt, e1, e2)
  let (<=) e1 e2 = Relop (Int Le, e1, e2)
  let (>=) e1 e2 = Relop (Int Ge, e1, e2)
  let neg e = Unop (Int Neg, e)
  let const i = Val (Int i)
end 

module RealInfix = struct 
  let (+) e1 e2  = Binop (Real Add, e1, e2) 
  let (-) e1 e2 = Binop (Real Sub, e1, e2)
  let ( * ) e1 e2 = Binop (Real Mul, e1, e2)
  let ( / ) e1 e2 = Binop (Real Div, e1, e2)
  let (<) e1 e2 = Relop (Real Lt, e1, e2)
  let (>) e1 e2 = Relop (Real Gt, e1, e2)
  let (<=) e1 e2 = Relop (Real Le, e1, e2)
  let (>=) e1 e2 = Relop (Real Ge, e1, e2)
  let neg e = Unop (Real Neg, e)
  let const i = Val (Real i)
end 


module I32Infix = struct 
  let (+) e1 e2  = Binop (I32 Add, e1, e2) 
  let (-) e1 e2 = Binop (I32 Sub, e1, e2)
  let ( * ) e1 e2 = Binop (I32 Mul, e1, e2)
  let ( < ) e1 e2 = Relop (I32 LtU, e1, e2)
  let ( <+ ) e1 e2 = Relop (I32 LtS, e1, e2)
  let ( > ) e1 e2 = Relop (I32 GtU, e1, e2)
  let ( >+ ) e1 e2 = Relop (I32 GtS, e1, e2)
  let ( <= ) e1 e2 = Relop (I32 LeU, e1, e2)
  let ( <=+ ) e1 e2 = Relop (I32 LeS, e1, e2)
  let ( >= ) e1 e2 = Relop (I32 GeU, e1, e2)
  let ( >=+ ) e1 e2 = Relop (I32 GeS, e1, e2)
  let neg e = Unop (I32 Not, e)
  let const i = BitVector.mk_val i `I32Type
end 

module I64Infix = struct 
  let (+) e1 e2  = Binop (I64 Add, e1, e2) 
  let (-) e1 e2 = Binop (I64 Sub, e1, e2)
  let ( * ) e1 e2 = Binop (I64 Mul, e1, e2)
  let ( < ) e1 e2 = Relop (I64 LtU, e1, e2)
  let ( <+ ) e1 e2 = Relop (I64 LtS, e1, e2)
  let ( > ) e1 e2 = Relop (I64 GtU, e1, e2)
  let ( >+ ) e1 e2 = Relop (I64 GtS, e1, e2)
  let ( <= ) e1 e2 = Relop (I64 LeU, e1, e2)
  let ( <=+ ) e1 e2 = Relop (I64 LeS, e1, e2)
  let ( >= ) e1 e2 = Relop (I64 GeU, e1, e2)
  let ( >=+ ) e1 e2 = Relop (I64 GeS, e1, e2)
  let neg e = Unop (I64 Not, e)
  let const i = BitVector.mk_val i `I64Type
end 

module F32Infix = struct 
  let (+) e1 e2  = Binop (F32 Add, e1, e2) 
  let (-) e1 e2 = Binop (F32 Sub, e1, e2)
  let ( * ) e1 e2 = Binop (F32 Mul, e1, e2)
  let (<) e1 e2 = Relop (F32 Lt, e1, e2)
  let (>) e1 e2 = Relop (F32 Gt, e1, e2)
  let (<=) e1 e2 = Relop (F32 Le, e1, e2)
  let (>=) e1 e2 = Relop (F32 Ge, e1, e2)
  let neg e = Unop (F32 Neg, e)
  let const i = FloatingPoint.mk_val i `F32Type
end 

module F64Infix = struct 
  let (+) e1 e2  = Binop (F64 Add, e1, e2) 
  let (-) e1 e2 = Binop (F64 Sub, e1, e2)
  let ( * ) e1 e2 = Binop (F64 Mul, e1, e2)
  let (<) e1 e2 = Relop (F64 Lt, e1, e2)
  let (>) e1 e2 = Relop (F64 Gt, e1, e2)
  let (<=) e1 e2 = Relop (F64 Le, e1, e2)
  let (>=) e1 e2 = Relop (F64 Ge, e1, e2)
  let neg e = Unop (F64 Neg, e)
  let const i = FloatingPoint.mk_val i `F64Type
end 
