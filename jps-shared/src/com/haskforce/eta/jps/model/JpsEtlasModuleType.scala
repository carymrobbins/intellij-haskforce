package com.haskforce.eta.jps.model

import org.jetbrains.jps.model.JpsDummyElement
import org.jetbrains.jps.model.ex.JpsElementTypeWithDummyProperties
import org.jetbrains.jps.model.module.JpsModuleType

/** Identifies an Etlas module for the external builder. */
class JpsEtlasModuleType
  extends JpsElementTypeWithDummyProperties
  with JpsModuleType[JpsDummyElement]

object JpsEtlasModuleType {
  val INSTANCE = new JpsEtlasModuleType
}
