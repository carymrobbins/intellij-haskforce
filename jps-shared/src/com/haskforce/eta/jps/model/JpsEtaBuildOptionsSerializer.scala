package com.haskforce.eta.jps.model

import com.intellij.util.xmlb.XmlSerializer
import org.jdom.Element
import org.jetbrains.jps.model.JpsProject
import org.jetbrains.jps.model.serialization.JpsProjectExtensionSerializer


/** Used for serializing the Eta build options to the file system. */
final class JpsEtaBuildOptionsSerializer extends JpsProjectExtensionSerializer(
  JpsEtaBuildOptionsConstants.ETA_BUILD_OPTIONS_FILE,
  JpsEtaBuildOptionsConstants.ETA_BUILD_OPTIONS_COMPONENT
) {

  override def loadExtension(project: JpsProject, componentTag: Element): Unit = {
    val ext = JpsEtaBuildOptionsExtension.getOrCreate(project)
    Option(XmlSerializer.deserialize(componentTag, classOf[EtaBuildOptions])).foreach { ext.setOptions }
  }

  override def saveExtension(project: JpsProject, componentTag: Element): Unit = {
    // TODO: Explain why this is a noop
  }
}
