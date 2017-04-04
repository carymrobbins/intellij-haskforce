package com.haskforce.project.template.stack

import javax.swing.JComponent

import com.intellij.framework.addSupport.FrameworkSupportInModuleConfigurable
import com.intellij.openapi.module.Module
import com.intellij.openapi.roots.{ModifiableModelsProvider, ModifiableRootModel}

class StackFrameworkConfigurable extends FrameworkSupportInModuleConfigurable {

  override def createComponent(): JComponent = null

  override def addSupport(
    module: Module,
    rootModel: ModifiableRootModel,
    modifiableModelsProvider: ModifiableModelsProvider
  ): Unit = {}
}
