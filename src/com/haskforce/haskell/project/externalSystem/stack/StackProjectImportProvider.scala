package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.service.project.wizard.AbstractExternalProjectImportProvider

class StackProjectImportProvider(builder: StackProjectImportBuilder)
  extends
    AbstractExternalProjectImportProvider(
      builder,
      StackManager.PROJECT_SYSTEM_ID
    )
