import Arith

#check DimensionalAnalysis.DCalc


#check `[DCalc| x ] 
#check `[DCalc| (x) ] 
#check `[DCalc| x * y ] 
#check `[DCalc| x / y ]
#check `[DCalc| x ^ 1/1 ]
#check `[DCalc| x^-1/1 ]
#check `[DCalc| x*y^2/1]
#eval `[DCalc| x^3/1*y^-2/1]

#eval `[DCalc| (x ^ 7/2)]
#eval DimensionalAnalysis.convert `[DCalc| x ^ 7/2 ]
#eval `[DCalc| (x ^ 7/2)^1/4]
#eval DimensionalAnalysis.simplify (DimensionalAnalysis.convert `[DCalc| (x ^ 7/2)^1/4])
#eval DimensionalAnalysis.simplify (DimensionalAnalysis.convert `[DCalc| x ^ 7/2 ] )
#eval DimensionalAnalysis.simplify (DimensionalAnalysis.convert `[DCalc| (x ^ 6/2)^1/1 ] )
#eval DimensionalAnalysis.convert `[DCalc| x^-3/1*y^2/1] 
#eval DimensionalAnalysis.simplify (DimensionalAnalysis.convert `[DCalc| x^-3/1*y^2/1] )

-- iso-80000-3 2006:3-1.1
def length := `[DCalc| m^1/1]

-- iso-80000-3 2006:3-1.2
def breadth := `[DCalc| length^1/1]

-- iso-80000-3 2006:3-1.3
def height := `[DCalc| length^1/1]

-- iso-80000-3 2006:3-1.5
def radius := `[DCalc| length^1/1]

-- iso-80000-3 2006:3-1.8
def lengthOfPath := `[DCalc| length^1/1]

-- iso-80000-3 2006:3-3
def area := `[DCalc| length * length]

-- iso-80000-3 2006:3-4
def volume := `[DCalc| area * length]

-- iso-80000-3 2006:3-5
def angle := `[DCalc| lengthOfPath / radius]

-- iso-80000-3:3.1
def time := `[DCalc| m^1/1]

-- iso-80000-4:4.1
def mass := `[DCalc| m^1/1]

#eval DimensionalAnalysis.simplify (DimensionalAnalysis.convert angle )

def main : IO Unit := do
  IO.println s!"angle={DimensionalAnalysis.simplify (DimensionalAnalysis.convert angle)}"

#eval main
