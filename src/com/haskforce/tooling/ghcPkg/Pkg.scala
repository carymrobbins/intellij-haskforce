package com.haskforce.tooling.ghcPkg

final case class Pkg(
  name: String,
  version: String,
  exposedModules: List[String]
)
