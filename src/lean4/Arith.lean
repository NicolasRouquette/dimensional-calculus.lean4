import Lean.Data.Rat
import Std.Data
open Std

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

structure DCalcFactor where
  (symbol: String)
  (exp: Lean.Rat)
  deriving Repr

instance : Inhabited DCalcFactor where
  default := DCalcFactor.mk "" (Lean.mkRat 1 1)

abbrev DCalcFactors := Array DCalcFactor

instance : ToString DCalcFactors where
  toString fs := s!"{fs.map repr}"

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

def applyPow (fs: DCalcFactors) (exp: Lean.Rat) : DCalcFactors :=
  fs.map (powerOf exp)

partial def applyMul (fs1: DCalcFactors) (fs2: DCalcFactors) : DCalcFactors :=
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
      let f12 := DCalcFactor.mk f1a.symbol (f1a.exp + f2.exp)
      let fs1a := (fs1.toSubarray 0 i).toArray
      let fs1b := (fs1.toSubarray (i+1)).toArray
      applyMul ((if 0 == f12.exp.num then fs1a else fs1a.append #[ f12 ]).append fs1b) f2tail

partial def applyDiv (fs1: DCalcFactors) (fs2: DCalcFactors) : DCalcFactors :=
  if 0 == fs2.size then
    fs1
  else
    let f2 : DCalcFactor := fs2.get! 0
    let f2tail := fs2.toSubarray 1
    let i1 : Option Nat := fs1.findIdx? fun f1 => f1.symbol == f2.symbol
    match i1 with
    | none =>
      applyMul (fs1.append #[ DCalcFactor.mk f2.symbol ( - f2.exp ) ]) f2tail
    | some i =>
      let f1a := fs1.get! i
      let f12 := DCalcFactor.mk f1a.symbol (f1a.exp - f2.exp)
      let fs1a := (fs1.toSubarray 0 i).toArray
      let fs1b := (fs1.toSubarray (i+1)).toArray
      applyMul ((if 0 == f12.exp.num then fs1a else fs1a.append #[ f12 ]).append fs1b) f2tail

end DCalcExpr

def convert : DCalc -> DCalcExpr
  | DCalc.symbol x => DCalcExpr.factors #[ (DCalcFactor.mk x (Lean.mkRat 1 1)) ]
  | DCalc.mul x y => DCalcExpr.mul (convert x) (convert y)
  | DCalc.div x y => DCalcExpr.div (convert x) (convert y)
  | DCalc.power x exp=> DCalcExpr.power (convert x) exp

def simplify : DCalcExpr -> DCalcFactors
  | DCalcExpr.factors fs => 
    fs
  | DCalcExpr.mul m1 m2 =>
    DCalcExpr.applyMul (simplify m1) (simplify m2)
  | DCalcExpr.div d1 d2 =>
    DCalcExpr.applyDiv (simplify d1) (simplify d2)
  | DCalcExpr.power x e => 
    DCalcExpr.applyPow (simplify x) e

structure Context where
  base : HashSet String
  derived : HashMap String DCalcFactors

namespace Context

def empty : Context := ⟨ HashSet.empty, HashMap.empty ⟩

instance : Inhabited Context where
  default := empty

partial def substitute (ctx: Context) (fs: DCalcFactors) : DCalcFactors :=
  if 0 == fs.size then
    fs
  else
    let v := fs.get! 0
    let tail := (fs.toSubarray 1).toArray
    if let some vs := ctx.derived.find? v.symbol then
      substitute ctx (DCalcExpr.applyMul (DCalcExpr.applyPow vs v.exp) tail)
    else
      let ts := substitute ctx tail
      DCalcExpr.applyMul #[ v ] ts

def reduce (ctx: Context) (symbol: String): Option DCalcFactors :=
  (ctx.derived.find? symbol).map (substitute ctx)

def withDerivation (ctx: Context) (symbol: String) (exp: DCalc) : Context := {
  if ctx.derived.contains symbol then
    panic! s!"Derived symbol already in the context: {symbol}"
  else
    ctx with derived := ctx.derived.insert symbol (simplify (convert exp))
}


def addDerivation (ctx: Context) (pair: String × DCalc) : Context := {
  if ctx.derived.contains pair.fst then
    panic! s!"Derived symbol already in the context: {pair.fst}"
  else
    ctx with derived := ctx.derived.insert pair.fst (simplify (convert pair.snd))
}

def mkContext (derivations: Array (String × DCalc)) : Context :=
  let ctx : Context := ⟨ HashSet.empty, HashMap.empty ⟩
  derivations.foldl addDerivation ctx

end Context

end DimensionalAnalysis
