import Lean.Data.Rat
import Std.Data
import Std.Data.HashSet
open Std

universe u
variable {α : Type u}

-- TODO: Where should these HashSet extensions be defined?

def ofList [BEq α] [Hashable α] (l : List α) : HashSet α :=
  l.foldl (init := HashSet.empty) (fun m p => m.insert p)

def union [BEq α] [Hashable α] (h1: HashSet α) (h2: HashSet α) : HashSet α :=
  h1.fold (init := h2) (fun h p => h.insert p)

def containsAll [BEq α] [Hashable α] (sup: HashSet α) (sub: HashSet α) : Bool :=
  sub.fold (init := true) (fun b p => b && sup.contains p)

-- TODO: Can this be generalized to a HashSet of α if there is LT.lt α?
instance : Repr (HashSet String) where
  reprPrec hs _ := 
    let as : Array String := hs.toArray.qsort (fun a b => a < b)
    as.toList.toString

-- Arithmetic for dimensional calculus
namespace DimensionalAnalysis

inductive DCalc : Type
  | symbol : String -> DCalc
  | mul : DCalc -> DCalc -> DCalc
  | div : DCalc -> DCalc -> DCalc
  | power : DCalc -> Lean.Rat -> DCalc
  deriving Repr

namespace DCalc

def symbols (dc: DCalc) : HashSet String :=
  match dc with
  | symbol s =>   HashSet.empty.insert s
  | mul l r =>    union (symbols l) (symbols r)
  | div l r =>    union (symbols l) (symbols r)
  | power l r =>  symbols l

end DCalc

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

instance : ToString DCalcFactor := ⟨fun pair =>  s!"{pair.fst}^{pair.snd}"⟩

instance DCalcFactors.repr : Repr DCalcFactors where
  reprPrec m _ := m.toList.toString

instance : ToString (String × DCalcFactors) := ⟨fun pair =>  pair.fst ++ "=" ++ (DCalcFactors.repr.reprPrec pair.snd 0).pretty⟩

instance : Repr (HashMap String DCalcFactors) where
  reprPrec m _ := m.toList.toString

def lt (f1: DCalcFactor) (f2: DCalcFactor): Bool := f1.fst < f2.fst

instance : ToString (Option DCalcFactors) := ⟨fun
  | none => "none"
  | (some fs) => "(some " ++ (fs.toArray.qsort lt).toList.toString ++ ")"⟩

namespace DCalcFactor

def mult (f: DCalcFactor) (e: Lean.Rat) : DCalcFactor :=
  ⟨ f.fst, (f.snd.mul e) ⟩

end DCalcFactor

inductive DCalcExpr : Type
  | factors: DCalcFactors -> DCalcExpr
  | mul : DCalcExpr -> DCalcExpr -> DCalcExpr
  | div : DCalcExpr -> DCalcExpr -> DCalcExpr
  | power : DCalcExpr -> Lean.Rat -> DCalcExpr
  deriving Repr

namespace DCalcExpr

def powerOf (exp: Lean.Rat) (f: DCalcFactor) : DCalcFactor := 
  ⟨ f.fst, (f.snd * exp) ⟩

def applyPow (fs: DCalcFactors) (exp: Lean.Rat) : DCalcFactors :=
  let ls : List DCalcFactor := fs.toList.map (powerOf exp)
  HashMap.ofList ls

def applyMulAux (fs1: DCalcFactors) (l2: List DCalcFactor): DCalcFactors :=
  match l2 with
  | List.nil =>
    fs1
  | List.cons f2 t2 =>
    match fs1.getOp f2.fst with
    | some (f1a : Lean.Rat) =>
      let f12 : DCalcFactor := ⟨ f2.fst, f1a + f2.snd ⟩
      let fs1a := fs1.erase f2.fst
      let fs1b := if 0 == f12.snd.num then fs1a else fs1a.insert f12.fst f12.snd
      applyMulAux fs1b t2
    | none =>
      let fs1b := fs1.insert f2.fst f2.snd
      applyMulAux fs1b t2

def applyMul (fs1: DCalcFactors) (fs2: DCalcFactors) : DCalcFactors :=
  applyMulAux fs1 fs2.toList

-- TODO: remove `partial` using well-founded recursion 
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
  deriving Repr

inductive ContextOrError : Type
  | error: String -> ContextOrError
  | context: Context -> ContextOrError
  deriving Repr

namespace Context

def empty : Context := ⟨ HashSet.empty, HashMap.empty ⟩

def scope (ctx: Context) : HashSet String :=
  ofList (ctx.base.toList ++ ctx.derived.toList.map Prod.fst)

instance : Inhabited Context where
  default := empty

def addBase (coe: ContextOrError) (b: String) : ContextOrError :=
  match coe with
  | ContextOrError.error msg => 
    ContextOrError.error msg
  | ContextOrError.context ctx =>
    if ctx.scope.contains b then
      ContextOrError.error s!"Base symbol '{b}' is already in the context!"
    else
      ContextOrError.context ⟨ ctx.base.insert b, ctx.derived ⟩

def addDerivation (coe: ContextOrError) (pair: String × DCalc) : ContextOrError :=
  match coe with
  | ContextOrError.error err => 
    ContextOrError.error err
  | ContextOrError.context ctx =>
    let sc : HashSet String := ctx.scope
    if sc.contains pair.fst then
      ContextOrError.error s!"Derived symbol '{pair.fst}' is already in the context!"
    else
      let ds : HashSet String := DCalc.symbols pair.snd
      if containsAll sc ds then
        ContextOrError.context ⟨ ctx.base, ctx.derived.insert pair.fst (simplify (convert pair.snd)) ⟩
      else
        let undefined : HashSet String := ds.fold (init := HashSet.empty) (fun u p => if sc.contains p then u else u.insert p)
        ContextOrError.error s!"The derivation of {pair.fst} refers to the following undefined symbols of the context: {undefined.toList.toString}"

def mkContext (base: Array String) (derivations: Array (String × DCalc)) : ContextOrError :=
  let withBases := base.foldl addBase (ContextOrError.context ⟨ HashSet.empty, HashMap.empty ⟩)
  derivations.foldl addDerivation withBases
  
-- TODO: remove `partial` using well-founded recursion 
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

end Context

namespace ContextOrError

def reduce (coe: ContextOrError) (symbol: String): Option DCalcFactors :=
  match coe with
  | ContextOrError.error msg => 
    none
  | ContextOrError.context ctx =>
    ctx.reduce symbol

end ContextOrError

end DimensionalAnalysis
