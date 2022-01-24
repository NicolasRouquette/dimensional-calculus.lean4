-- Arithmetic for dimensional calculus
namespace DimensionalAnalysis

inductive DCalc : Type
  | symbol : String -> DCalc
  | mul : DCalc -> DCalc -> DCalc
  | div : DCalc -> DCalc -> DCalc
  | power : DCalc -> Bool -> Nat -> Nat -> DCalc
  deriving Repr

declare_syntax_cat dcalc

syntax ident : dcalc -- DCalc.symbol
syntax:70 dcalc:70 "*" dcalc:71 : dcalc -- DCalc.mul
syntax:80 dcalc:80 "/" dcalc:81 : dcalc -- DCalc.div
syntax "(" dcalc ")" : dcalc
syntax:90 dcalc:90 "^" num "/" num : dcalc -- DCalc.power
syntax:90 dcalc:90 "^" "-" num "/" num : dcalc -- DCalc.power

-- auxiliary notation for translating `dcalc` into `term`
syntax "`[DCalc| " dcalc "]" : term

macro_rules
  | `(`[DCalc| $x:ident ]) => `(DCalc.symbol $(Lean.quote (toString x.getId)))
  | `(`[DCalc| $x:dcalc * $y:dcalc ]) => `(DCalc.mul `[DCalc| $x] `[DCalc| $y])
  | `(`[DCalc| $x:dcalc / $y:dcalc ]) => `(DCalc.div `[DCalc| $x] `[DCalc| $y])
  | `(`[DCalc| ($x:dcalc) ]) => `(`[DCalc| $x ])
  | `(`[DCalc| $x:dcalc ^ $n:numLit / $d:numLit ]) => `(DCalc.power `[DCalc| $x] true $n $d)
  | `(`[DCalc| $x:dcalc ^ - $n:numLit / $d:numLit ]) => `(DCalc.power `[DCalc| $x] false $n $d)

#check `[DCalc| x ]           -- DCalc.symbol "x" : DCalc
#check `[DCalc| (x) ]         -- DCalc.symbol "x" : DCalc
#check `[DCalc| x * y ]       -- DCalc.mul (DCalc.symbol "x") (DCalc.symbol "y") : DCalc
#check `[DCalc| x / y ]       -- DCalc.div (DCalc.symbol "x") (DCalc.symbol "y") : DCalc
#check `[DCalc| x ^ 1/1 ]     -- DCalc.power (DCalc.symbol "x") true 1 1 : DCalc
#check `[DCalc| x^-1/1 ]      -- DCalc.power (DCalc.symbol "x") false 1 1 : DCalc
#check `[DCalc| x*y^2/1]      -- DCalc.mul (DCalc.symbol "x") (DCalc.power (DCalc.symbol "y") true 2 1) : DCalc
#check `[DCalc| x^3/1*y^2/1]  -- DCalc.mul (DCalc.power (DCalc.symbol "x") true 3 1) (DCalc.power (DCalc.symbol "y") true 2 1) : DCalc

structure DCalcFactor where
  (symbol: String)
  (isPositive: Bool)
  (num: Nat)
  (den: Nat)
  deriving Repr

inductive DCalcExpr : Type
  | factors: Array DCalcFactor -> DCalcExpr
  | mul : DCalcExpr -> DCalcExpr -> DCalcExpr
  | div : DCalcExpr -> DCalcExpr -> DCalcExpr
  | power : DCalcExpr -> Bool -> Nat -> Nat -> DCalcExpr
  deriving Repr

def convert : DCalc -> DCalcExpr
  | DCalc.symbol x => DCalcExpr.factors #[ (DCalcFactor.mk x true 1 1) ]
  | DCalc.mul x y => DCalcExpr.mul (convert x) (convert y)
  | DCalc.div x y => DCalcExpr.div (convert x) (convert y)
  | DCalc.power x f n d => DCalcExpr.power (convert x) f n d

def simplify : DCalcExpr -> DCalcExpr
  | DCalcExpr.factors fs => 
    DCalcExpr.factors fs
  | DCalcExpr.power x f d n => 
    match x with
    | DCalcExpr.factors fs => 
      DCalcExpr.power (DCalcExpr.factors fs) f d n
    | x' => 
      DCalcExpr.power x' f d n
  | d => d

#eval simplify (convert `[DCalc| x ^ 7/2 ] )
#eval simplify (convert `[DCalc| x ^ 6/2 ] )
#eval simplify (convert `[DCalc| x^3/1*y^2/1] )


end DimensionalAnalysis
