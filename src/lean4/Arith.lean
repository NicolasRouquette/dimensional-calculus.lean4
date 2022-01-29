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

abbrev DCalcFactor := String × Lean.Rat

abbrev DCalcFactors := HashMap String Lean.Rat

def lt (f1: DCalcFactor) (f2: DCalcFactor): Bool := 
  f1.fst.data.le f2.fst.data

instance : ToString (Option DCalcFactors) := ⟨fun
  | none => "none"
  | (some fs) => "(some " ++ (fs.toArray.qsort lt).toList.toString ++ ")"⟩

def ToString (pair: String × Lean.Rat) : String :=
  s!"{pair.fst}^{pair.snd}"
namespace DCalcFactor

def mult (f: DCalcFactor) (e: Lean.Rat) : DCalcFactor :=
  ⟨ f.fst, (f.snd.mul e) ⟩

end DCalcFactor

inductive DCalcExpr : Type
  | factors: DCalcFactors -> DCalcExpr
  | mul : DCalcExpr -> DCalcExpr -> DCalcExpr
  | div : DCalcExpr -> DCalcExpr -> DCalcExpr
  | power : DCalcExpr -> Lean.Rat -> DCalcExpr

namespace DCalcExpr

def powerOf (exp: Lean.Rat) (f: DCalcFactor) : DCalcFactor := 
  ⟨ f.fst, (f.snd * exp) ⟩

def applyPow (fs: DCalcFactors) (exp: Lean.Rat) : DCalcFactors :=
  let ls : List DCalcFactor := fs.toList.map (powerOf exp)
  HashMap.ofList ls

partial def applyMul (fs1: DCalcFactors) (fs2: DCalcFactors) : DCalcFactors :=
  if 0 == fs2.size then
    fs1
  else
    let l2 : List DCalcFactor := fs2.toList
    let f2 := l2.head!
    let f2tail := HashMap.ofList l2.tail!
    match fs1.getOp f2.fst with
    | none =>
      applyMul (fs1.insert f2.fst f2.snd) f2tail
    | some (f1a : Lean.Rat) =>
      let f12 : DCalcFactor := ⟨ f2.fst, f1a + f2.snd ⟩
      let fs1a := fs1.erase f2.fst
      let fs1b := if 0 == f12.snd.num then fs1a else fs1a.insert f12.fst f12.snd
      applyMul fs1b f2tail

partial def applyDiv (fs1: DCalcFactors) (fs2: DCalcFactors) : DCalcFactors :=
  if 0 == fs2.size then
    fs1
  else
    let l2 : List DCalcFactor := fs2.toList
    let f2 := l2.head!
    let f2tail := HashMap.ofList l2.tail!
    match fs1.getOp f2.fst with
    | none =>
      applyDiv (fs1.insert f2.fst (-f2.snd)) f2tail
    | some (f1a : Lean.Rat) =>
      let f12 : DCalcFactor := ⟨ f2.fst, f1a - f2.snd ⟩
      let fs1a := fs1.erase f2.fst
      let fs1b := if 0 == f12.snd.num then fs1a else fs1a.insert f12.fst f12.snd
      applyDiv fs1b f2tail

end DCalcExpr

def convert : DCalc -> DCalcExpr
  | DCalc.symbol x => DCalcExpr.factors (HashMap.ofList (List.cons ⟨ x, Lean.mkRat 1 1 ⟩ List.nil))
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
    let v := fs.toList.head!
    let tail := HashMap.ofList fs.toList.tail!
    if let some vs := ctx.derived.find? v.fst then
      substitute ctx (DCalcExpr.applyMul (DCalcExpr.applyPow vs v.snd) tail)
    else
      let ts := substitute ctx tail
      DCalcExpr.applyMul (HashMap.ofList (List.cons ⟨ v.fst, v.snd ⟩ List.nil)) ts

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
