package com.haskforce.haskell.project.template

import javax.swing.Icon

import com.haskforce.{HaskellIcons, HaskellModuleBuilder}
import com.intellij.ide.util.projectWizard.AbstractModuleBuilder
import com.intellij.openapi.ui.ValidationInfo
import com.intellij.platform.ProjectTemplate

/**
 * Created by crobbins on 12/7/16.
 */
class HaskellProjectTemplate extends ProjectTemplate {

  override def getName: String = "Haskell"

  override def getIcon: Icon = HaskellIcons.FILE

  override def getDescription: String = "Haskell module with GHC support"

  override def validateSettings(): ValidationInfo = null

  override def createModuleBuilder(): AbstractModuleBuilder = new HaskellModuleBuilder
}
