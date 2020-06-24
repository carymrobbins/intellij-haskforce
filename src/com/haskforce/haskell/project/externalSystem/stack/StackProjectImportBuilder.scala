package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.HaskellIcons
import com.intellij.ide.util.projectWizard.WizardContext
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.project.ProjectDataManager
import com.intellij.openapi.externalSystem.service.project.wizard.AbstractExternalProjectImportBuilder
import com.intellij.openapi.project.Project
import javax.swing.Icon

class StackProjectImportBuilder extends
    AbstractExternalProjectImportBuilder[StackSettingsControl](
      ProjectDataManager.getInstance(),
      () => StackSettingsControl.forDefaultProject(),
      StackManager.PROJECT_SYSTEM_ID
    ) {

  override def getName: String = "Stack"

  override def getIcon: Icon = HaskellIcons.FILE

  override def doPrepare(context: WizardContext): Unit = {
    getControl(context.getProject).setLinkedProjectPath(getFileToImport)
  }

  override def beforeCommit(dataNode: DataNode[ProjectData], project: Project): Unit = {}

  override def getExternalProjectConfigToUse(file: File): File = {
    // TODO: The 'stack.yaml' should be configurable...somehow?
    file.listFiles(_.getName == "stack.yaml").headOption.orNull
  }

  override def applyExtraSettings(context: WizardContext): Unit = {}
}
