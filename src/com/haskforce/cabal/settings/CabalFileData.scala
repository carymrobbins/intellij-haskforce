package com.haskforce.cabal.settings

/** Container for built Cabal project info for generating a new .cabal file. */
final case class CabalFileData(
  packageVersion: String,
  synopsis: String,
  homepage: String,
  author: String,
  maintainer: String,
  category: String,
  cabalVersion: String,
  componentType: CabalComponentType,
  sourceDir: String,
  language: String
)
