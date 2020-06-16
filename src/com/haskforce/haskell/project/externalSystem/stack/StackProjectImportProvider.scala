package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.service.project.wizard.AbstractExternalProjectImportProvider
import com.intellij.projectImport.ProjectImportBuilder

class StackProjectImportProvider
  extends
    AbstractExternalProjectImportProvider(
      StackManager.PROJECT_SYSTEM_ID
    ) {

  override def doGetBuilder(): ProjectImportBuilder[_] = {
    new StackProjectImportBuilder
  }
}
