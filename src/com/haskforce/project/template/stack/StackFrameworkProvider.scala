package com.haskforce.project.template.stack

import com.intellij.framework.FrameworkTypeEx
import com.intellij.framework.addSupport.{FrameworkSupportInModuleConfigurable, FrameworkSupportInModuleProvider}
import com.intellij.ide.util.frameworkSupport.FrameworkSupportModel
import com.intellij.ide.util.projectWizard.ModuleBuilder
import com.intellij.openapi.module.ModuleType

class StackFrameworkProvider extends FrameworkSupportInModuleProvider {

  override def isEnabledForModuleType(moduleType: ModuleType[_ <: ModuleBuilder]): Boolean = {
    true // TODO: Check `== HaskellModuleType.getInstance` ? What about Eta?
  }

  override def getFrameworkType: FrameworkTypeEx = StackFrameworkType.INSTANCE

  override def createConfigurable(
    model: FrameworkSupportModel
  ): FrameworkSupportInModuleConfigurable = new StackFrameworkConfigurable
}
