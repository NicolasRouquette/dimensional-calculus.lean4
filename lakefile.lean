import Lake
open System Lake DSL

package Arith where
  packagesDir := "build.lean4_packages"
  srcDir := "src" / "lean4"
  buildDir := "build.lean4"
    
@[defaultTarget]
lean_exe dc where
  root := `Main
