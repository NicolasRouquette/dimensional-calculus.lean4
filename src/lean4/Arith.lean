import Lean.Data.Rat

-- Arithmetic for dimensional calculus
namespace DimensionalAnalysis

inductive DCalc : Type
  | symbol : String -> DCalc
  | mul : DCalc -> DCalc -> DCalc
  | div : DCalc -> DCalc -> DCalc
  | power : DCalc -> Lean.Rat -> DCalc
  deriving Repr

declare_syntax_cat dcalc

syntax ident : dcalc
syntax dcalc "*" dcalc : dcalc
syntax dcalc "/" dcalc : dcalc
syntax "(" dcalc ")" : dcalc
syntax dcalc "^" num "/" num : dcalc
syntax dcalc "^" "-" num "/" num : dcalc

-- auxiliary notation for translating `dcalc` into `term`
syntax "`[DCalc| " dcalc "]" : term

macro_rules
  | `(`[DCalc| $x:ident ]) => `(DCalc.symbol $(Lean.quote (toString x.getId)))
  | `(`[DCalc| $x:dcalc * $y:dcalc ]) => `(DCalc.mul `[DCalc| $x] `[DCalc| $y])
  | `(`[DCalc| $x:dcalc / $y:dcalc ]) => `(DCalc.div `[DCalc| $x] `[DCalc| $y])
  | `(`[DCalc| ($x:dcalc) ]) => `(`[DCalc| $x ])
  | `(`[DCalc| $x:dcalc ^ $n:numLit / $d:numLit ]) => `(DCalc.power `[DCalc| $x] (Lean.mkRat $n $d))
  | `(`[DCalc| $x:dcalc ^ - $n:numLit / $d:numLit ]) => `(DCalc.power `[DCalc| $x] (Lean.mkRat ( - $n ) $d))

#check `[DCalc| x ] 
#check `[DCalc| (x) ] 
#check `[DCalc| x * y ] 
#check `[DCalc| x / y ]
#check `[DCalc| x ^ 1/1 ]
#check `[DCalc| x^-1/1 ]
#check `[DCalc| x*y^2/1]
#eval `[DCalc| x^3/1*y^-2/1]

structure DCalcFactor where
  (symbol: String)
  (exp: Lean.Rat)
  deriving Repr

instance : Inhabited DCalcFactor where
  default := DCalcFactor.mk "" (Lean.mkRat 1 1)
namespace DCalcFactor

def mult (f: DCalcFactor) (e: Lean.Rat) : DCalcFactor :=
  DCalcFactor.mk f.symbol (f.exp.mul e)

end DCalcFactor

inductive DCalcExpr : Type
  | factors: Array DCalcFactor -> DCalcExpr
  | mul : DCalcExpr -> DCalcExpr -> DCalcExpr
  | div : DCalcExpr -> DCalcExpr -> DCalcExpr
  | power : DCalcExpr -> Lean.Rat -> DCalcExpr
  deriving Repr

namespace DCalcExpr

def powerOf (exp: Lean.Rat) (f: DCalcFactor) : DCalcFactor := 
  DCalcFactor.mk f.symbol (f.exp * exp)

def applyPow (fs: Array DCalcFactor) (exp: Lean.Rat) : Array DCalcFactor :=
  fs.map (powerOf exp)

partial def applyMul (fs1: Array DCalcFactor) (fs2: Array DCalcFactor) : Array DCalcFactor :=
  if 0 == fs2.size then
    fs1
  else
    let f2 : DCalcFactor := fs2.get! 0
    let f2tail := fs2.toSubarray 1
    let i1 : Option Nat := fs1.findIdx? fun f1 => f1.symbol == f2.symbol
    match i1 with
    | none =>
      applyMul (fs1.append #[ f2 ]) f2tail
    | some i =>
      let f1a := fs1.get! i
      let f1b := DCalcFactor.mk f1a.symbol (f1a.exp + f2.exp)
      let fs1a := (fs1.toSubarray 0 i).toArray
      let fs1b := (fs1.toSubarray (i+1)).toArray
      applyMul ((fs1a.append #[ f1b ]).append fs1b) f2tail

partial def applyDiv (fs1: Array DCalcFactor) (fs2: Array DCalcFactor) : Array DCalcFactor :=
  if 0 == fs2.size then
    fs1
  else
    let f2 : DCalcFactor := fs2.get! 0
    let f2m : DCalcFactor := DCalcFactor.mk f2.symbol ( - f2.exp )
    let f2tail := fs2.toSubarray 1
    let i1 : Option Nat := fs1.findIdx? fun f1 => f1.symbol == f2.symbol
    match i1 with
    | none =>
      applyMul (fs1.append #[ f2m ]) f2tail
    | some i =>
      let f1a := fs1.get! i
      let f1b := DCalcFactor.mk f1a.symbol (f1a.exp + f2m.exp)
      let fs1a := (fs1.toSubarray 0 i).toArray
      let fs1b := (fs1.toSubarray (i+1)).toArray
      applyMul ((fs1a.append #[ f1b ]).append fs1b) f2tail

end DCalcExpr

def convert : DCalc -> DCalcExpr
  | DCalc.symbol x => DCalcExpr.factors #[ (DCalcFactor.mk x (Lean.mkRat 1 1)) ]
  | DCalc.mul x y => DCalcExpr.mul (convert x) (convert y)
  | DCalc.div x y => DCalcExpr.div (convert x) (convert y)
  | DCalc.power x exp=> DCalcExpr.power (convert x) exp

def simplify : DCalcExpr -> Array DCalcFactor
  | DCalcExpr.factors fs => 
    fs
  | DCalcExpr.mul m1 m2 =>
    DCalcExpr.applyMul (simplify m1) (simplify m2)
  | DCalcExpr.div d1 d2 =>
    DCalcExpr.applyDiv (simplify d1) (simplify d2)
  | DCalcExpr.power x e => 
    DCalcExpr.applyPow (simplify x) e

#eval `[DCalc| (x ^ 7/2)]
#eval convert `[DCalc| x ^ 7/2 ]
#eval `[DCalc| (x ^ 7/2)^1/4]
#eval simplify (convert `[DCalc| (x ^ 7/2)^1/4])
#eval simplify (convert `[DCalc| x ^ 7/2 ] )
#eval simplify (convert `[DCalc| (x ^ 6/2)^1/1 ] )
#eval convert `[DCalc| x^-3/1*y^2/1] 
#eval simplify (convert `[DCalc| x^-3/1*y^2/1] )


end DimensionalAnalysis
