import Arith
import Std.Data
open Std
open DimensionalAnalysis

-- iso-80000-3 2006:3-1.1
def length := `[DCalc| length^1/1 ]

-- iso-80000-3 2006:3-7
def time := `[DCalc| time^1/1]

-- iso-80000-4:4.1
def mass := `[DCalc| m^1/1]

def isq : Context :=
  Context.mkContext #[
    -- iso-80000-3 2006:3-1.2
    ("breadth", `[DCalc| length^1/1 ]),

    -- iso-80000-3 2006:3-1.3
    ("height", `[DCalc| length^1/1 ]),

    -- iso-80000-3 2006:3-1.5
    ("radius", `[DCalc| length^1/1 ]),

    -- iso-80000-3 2006:3-1.8
    ( "lengthOfPath", `[DCalc| length^1/1 ]),

    -- iso-80000-3 2006:3-3
    ("area", `[DCalc| length * length ]),

    -- iso-80000-3 2006:3-4
    ("volume", `[DCalc| area * length ]),

    -- iso-80000-3 2006:3-5
    ("angle", `[DCalc| lengthOfPath / radius ]),

    -- iso-80000-3 2006:3-8.1
    ("velocity", `[DCalc| length / time ]),

    -- iso-80000-3 2006:3-9.1
    ("acceleration", `[DCalc| velocity / time ]),

    -- iso-80000-4 2006:4-6
    ("momentum", `[DCalc| mass * velocity ]),

    -- iso-80000-4 2006:4-9.1
    ("force", `[DCalc| momentum / time ]),

    -- iso-80000-4 2006:4-15.1
    ("pressure", `[DCalc| force / area ]),

    ("pressure/mass", `[DCalc| pressure / mass ]),

    -- iso-80000-4 2006:4-26
    ("power", `[DCalc| force * velocity ]),

    -- iso-80000-4 2006:4-28
    ("power.efficiency", `[DCalc| power / power ])
  ]

def main : IO Unit := do
  -- area=(some #[{ symbol := "length", exp := 2 }])
  IO.println s!"area={isq.reduce "area"}"

  -- angle=(some #[{ symbol := "length", exp := 0 }])
  IO.println s!"angle={isq.reduce "angle"}"

  -- velocity=(some #[{ symbol := "length", exp := 1 }, { symbol := "time", exp := -1 }])
  IO.println s!"velocity={isq.reduce "velocity"}"

  -- acceleration=(some #[{ symbol := "length", exp := 1 }, { symbol := "time", exp := -2 }])
  IO.println s!"acceleration={isq.reduce "acceleration"}"

  -- momentum=(some #[{ symbol := "mass", exp := 1 }, { symbol := "length", exp := 1 }, { symbol := "time", exp := -1 }])
  IO.println s!"momentum={isq.reduce "momentum"}"

  -- force=(some #[{ symbol := "mass", exp := 1 }, { symbol := "length", exp := 1 }, { symbol := "time", exp := -2 }])
  IO.println s!"force={isq.reduce "force"}"

  -- pressure=(some #[{ symbol := "mass", exp := 1 }, { symbol := "length", exp := -1 }, { symbol := "time", exp := -2 }])
  IO.println s!"pressure={isq.reduce "pressure"}"

  IO.println s!"pressure/mass={isq.reduce "pressure/mass"}"

  -- power=(some #[{ symbol := "mass", exp := 1 }, { symbol := "length", exp := 2 }, { symbol := "time", exp := -3 }])
  IO.println s!"power={isq.reduce "power"}"

  -- power.efficiency=(some #[{ symbol := "mass", exp := 0 }, { symbol := "length", exp := 0 }, { symbol := "time", exp := 0 }])
  IO.println s!"power.efficiency={isq.reduce "power.efficiency"}"

#eval main
