import Lake
open Lake DSL

package DA where
  packagesDir := "build.lean4_packages"
  srcDir := "src" / "lean4"
  buildDir := "build.lean4"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

@[defaultTarget]
lean_exe da where
  root := `DA.Main
