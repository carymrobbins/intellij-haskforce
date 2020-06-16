package com.haskforce.haskell.project.externalSystem.stack

final case class PackageConfigAssoc(
  packageDir: String,
  packageConfig: PackageConfig
)
