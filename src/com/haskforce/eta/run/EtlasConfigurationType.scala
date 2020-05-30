package com.haskforce.eta.run

import com.haskforce.HaskellIcons
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationType, ConfigurationTypeBase}

class EtlasConfigurationType extends ConfigurationTypeBase(
  "Etlas Task Configuration",
  "Etlas Task",
  "Execute an Etlas task",
  HaskellIcons.ETA_FILE
) {
  override val getConfigurationFactories: Array[ConfigurationFactory] = {
    Array(EtlasConfigurationFactory)
  }
}

object EtlasConfigurationType {
  val INSTANCE: EtlasConfigurationType =
    ConfigurationType.CONFIGURATION_TYPE_EP
      .findExtensionOrFail(classOf[EtlasConfigurationType])
}
