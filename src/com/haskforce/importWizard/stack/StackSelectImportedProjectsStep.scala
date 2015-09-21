package com.haskforce.importWizard.stack

import javax.swing.Icon

import com.intellij.ide.util.projectWizard.WizardContext
import com.intellij.projectImport.SelectImportedProjectsStep

import com.haskforce.HaskellIcons

/**
 * Provides a list of stack packages that the user can import as modules.
 */
class StackSelectImportedProjectsStep(context: WizardContext)
extends SelectImportedProjectsStep[StackYaml.Package](context) {
  override def getElementText(item: StackYaml.Package): String = item.path
  override def getElementIcon(item: StackYaml.Package): Icon = HaskellIcons.FILE
}
