import Lake
open System Lake DSL

package Arith where
  packagesDir := FilePath.mk "build.lean4_packages"
  srcDir := FilePath.mk "src" / "lean4"
  buildDir := FilePath.mk "build.lean4"
  dependencies := #[{
    name := `mathlib
    src := Source.git "https://github.com/leanprover-community/mathlib4.git" "master"
  }]
