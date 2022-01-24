-- Arithmetic for dimensional calculus
namespace DimensionalAnalysis

structure Rational where
  (isPositive: Bool)
  (num: Nat)
  (den: Nat)
  deriving Repr

namespace Rational

def simplify (r: Rational) : Rational :=
  let c := Nat.gcd r.num r.den
  Rational.mk r.isPositive (r.num / c) (r.den / c)

def multiply (r1 r2: Rational) : Rational :=
  let n12 := Nat.mul r1.num r2.num
  let d12 := Nat.mul r1.den r2.den
  let c := Nat.gcd n12 d12
  let f := r1.isPositive && r2.isPositive
  Rational.mk f (n12/c) (d12/c)

def divide (r1 r2: Rational) : Rational :=
  multiply r1 (Rational.mk r2.isPositive r2.den r2.num)

#eval multiply (Rational.mk true 4 7) (Rational.mk false 21 3) 

end Rational

inductive DCalc : Type
  | symbol : String -> DCalc
  | mul : DCalc -> DCalc -> DCalc
  | div : DCalc -> DCalc -> DCalc
  | power : DCalc -> Rational -> DCalc
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
  | `(`[DCalc| $x:dcalc ^ $n:numLit / $d:numLit ]) => `(DCalc.power `[DCalc| $x] (Rational.mk true $n $d))
  | `(`[DCalc| $x:dcalc ^ - $n:numLit / $d:numLit ]) => `(DCalc.power `[DCalc| $x] (Rational.mk false $n $d))

#check `[DCalc| x ] 
#check `[DCalc| (x) ] 
#check `[DCalc| x * y ] 
#check `[DCalc| x / y ]
#check `[DCalc| x ^ 1/1 ]
#check `[DCalc| x^-1/1 ]
#check `[DCalc| x*y^2/1]
#check `[DCalc| x^3/1*y^2/1]

structure DCalcFactor where
  (symbol: String)
  (exp: Rational)
  deriving Repr

inductive DCalcExpr : Type
  | factors: Array DCalcFactor -> DCalcExpr
  | mul : DCalcExpr -> DCalcExpr -> DCalcExpr
  | div : DCalcExpr -> DCalcExpr -> DCalcExpr
  | power : DCalcExpr -> Rational -> DCalcExpr
  deriving Repr

def convert : DCalc -> DCalcExpr
  | DCalc.symbol x => DCalcExpr.factors #[ (DCalcFactor.mk x (Rational.mk true 1 1)) ]
  | DCalc.mul x y => DCalcExpr.mul (convert x) (convert y)
  | DCalc.div x y => DCalcExpr.div (convert x) (convert y)
  | DCalc.power x exp=> DCalcExpr.power (convert x) exp

def simplify : DCalcExpr -> DCalcExpr
  | DCalcExpr.factors fs => 
    DCalcExpr.factors fs
  | DCalcExpr.power x e => 
    match x with
    | DCalcExpr.factors fs => 
      DCalcExpr.power (DCalcExpr.factors fs) e
    | x' => 
      DCalcExpr.power x' e
  | d => d

#eval simplify (convert `[DCalc| x ^ 7/2 ] )
#eval simplify (convert `[DCalc| x ^ 6/2 ] )
#eval simplify (convert `[DCalc| x^3/1*y^2/1] )


end DimensionalAnalysis
