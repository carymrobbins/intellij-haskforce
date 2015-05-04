package com.haskforce.cabal.settings

import com.intellij.openapi.module.Module

/**
 * Data class to pass arguments to cabal init.
 */
sealed case class AddCabalPackageOptions(
  maybeModule: Option[Module],
  packageName: String,
  packageVersion: String,
  buildType: CabalBuildType,
  rootDir: String,
  sourceDir: String,
  cabalVersion: String,
  license: Option[String],
  author: Option[String],
  email: Option[String],
  homepage: Option[String],
  synopsis: Option[String],
  category: Option[String],
  language: String,
  generateComments: Boolean
)

/**
 * ADT representing possible Cabal build types.
 */
sealed trait CabalBuildType
object CabalBuildType {
  case object Library extends CabalBuildType
  case object Executable extends CabalBuildType
}
