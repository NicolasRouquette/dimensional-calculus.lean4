import Arith
import Std.Data
open Std
open DimensionalAnalysis

#check DimensionalAnalysis.DCalc

def derived0 : Context := AssocList.empty

-- iso-80000-3 2006:3-1.1
def length := `[DCalc| length^1/1 ]

-- iso-80000-3 2006:3-1.2
def derived1 := withDerivation derived0 "breadth" `[DCalc| length^1/1 ]

-- iso-80000-3 2006:3-1.3
def derived2 := withDerivation derived1 "height" `[DCalc| length^1/1 ]

-- iso-80000-3 2006:3-1.5
def derived3 := withDerivation derived2 "radius" `[DCalc| length^1/1 ]

-- iso-80000-3 2006:3-1.8
def derived4 := withDerivation derived3 "lengthOfPath" `[DCalc| length^1/1 ]

-- iso-80000-3 2006:3-3
def derived5 := withDerivation derived4 "area" `[DCalc| length * length ]

-- iso-80000-3 2006:3-4
def derived6 := withDerivation derived5 "volume" `[DCalc| area * length ]

-- iso-80000-3 2006:3-5
def derived7 := withDerivation derived6 "angle" `[DCalc| lengthOfPath * radius ]

-- iso-80000-3 2006:3-7
def time := `[DCalc| time^1/1]

-- iso-80000-3 2006:3-8.1
def derived8 := withDerivation derived7 "velocity" `[DCalc| length / time ]

-- iso-80000-3 2006:3-9.1
def derived9 := withDerivation derived8 "acceleration" `[DCalc| velocity / time ]

-- iso-80000-4:4.1
def mass := `[DCalc| m^1/1]

-- iso-80000-4 2006:4-6
def derived10 := withDerivation derived9 "momentum" `[DCalc| mass * velocity ]

-- iso-80000-4 2006:4-9.1
def derived11 := withDerivation derived10 "force" `[DCalc| momentum / time ]

-- iso-80000-4 2006:4-15.1
def derived12 := withDerivation derived11 "pressure" `[DCalc| force / area ]

-- iso-80000-4 2006:4-26
def derived13 := withDerivation derived12 "power" `[DCalc| force * velocity ]

def allDerivations := derived13

#eval simplify (convert `[DCalc| force / area ])
def main : IO Unit := do
  IO.println s!"area={reduce allDerivations "area"}"
  IO.println s!"velocity={reduce allDerivations "velocity"}"
  IO.println s!"acceleration={reduce allDerivations "acceleration"}"
  IO.println s!"momentum={reduce allDerivations "momentum"}"
  IO.println s!"force={reduce allDerivations "force"}"
  IO.println s!"pressure={reduce allDerivations "pressure"}"
  IO.println s!"power={reduce allDerivations "power"}"

#eval main
