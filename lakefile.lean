import Lake
open Lake DSL

package Arith {
  srcDir := "src" / "lean4"
}

lean_lib Arith {
}

@[defaultTarget]
lean_exe arith {
  root := `Main
}
