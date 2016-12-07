package com.haskforce.jps.model

import org.jdom.Element
import org.jetbrains.annotations.Nullable
import org.jetbrains.jps.model.module.JpsModuleType
import org.jetbrains.jps.model.serialization.module.JpsModulePropertiesSerializer
import org.jetbrains.jps.model.{JpsDummyElement, JpsElementFactory}

/**
 * Created by crobbins on 4/4/17.
 */
class JpsDummyModulePropertiesSerializer(
  typ: JpsModuleType[JpsDummyElement],
  typeId: String,
  @Nullable componentName: String
) extends JpsModulePropertiesSerializer[JpsDummyElement](typ, typeId, componentName) {

  def this(typ: JpsModuleType[JpsDummyElement], typeId: String) = {
    this(typ, typeId, null)
  }

  override def saveProperties(properties: JpsDummyElement, componentElement: Element): Unit = {}

  override def loadProperties(componentElement: Element): JpsDummyElement = {
    JpsElementFactory.getInstance.createDummyElement
  }
}
