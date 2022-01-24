import Lake
open System Lake DSL

package test where
  packagesDir := FilePath.mk "build.lean4_packages"
  srcDir := FilePath.mk "src" / "lean4"
  buildDir := FilePath.mk "build.lean4"
