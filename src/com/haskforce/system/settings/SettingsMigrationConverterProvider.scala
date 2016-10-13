package com.haskforce.system.settings

import java.io.File
import java.util

import com.haskforce.jps.model.JpsHaskellBuildOptionsSerializer
import com.intellij.conversion.{ConversionContext, ConverterProvider, ProjectConverter}
import org.jdom.Element

import scala.collection.JavaConverters._

/**
  */
class SettingsMigrationConverterProvider extends ConverterProvider("haskforce-Compiler-Settings-Converter") {
  override def createConverter(context: ConversionContext): ProjectConverter = {
    new SettingsMigrationConverter(context)
  }

  override def getConversionDescription: String = "Haskforce Compiler Settings Migration"
}

class SettingsMigrationConverter(context: ConversionContext) extends ProjectConverter {
  override def isConversionNeeded: Boolean = {
    val componentElement: Element = context.getCompilerSettings.getComponentElement(JpsHaskellBuildOptionsSerializer.HASKELL_BUILD_OPTIONS_COMPONENT_NAME)
    componentElement.getChild("version") == null
  }

  override def processingFinished(): Unit = {
    val componentElement: Element = context.getCompilerSettings.getComponentElement(JpsHaskellBuildOptionsSerializer.HASKELL_BUILD_OPTIONS_COMPONENT_NAME)
    if (componentElement.getChild("version") == null) {
      val versionElement: Element = new Element("version").setText("0")
      componentElement.addContent(versionElement)
    }
  }

  override def getAdditionalAffectedFiles: util.Collection[File] = {
    List(context.getCompilerSettings.getFile).asJavaCollection
  }
}
