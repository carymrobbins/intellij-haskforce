package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.importing.AbstractOpenProjectProvider
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

class StackOpenProjectProvider extends AbstractOpenProjectProvider {

  override def isProjectFile(virtualFile: VirtualFile): Boolean = {
    // TODO: Do we need to be more robust; e.g. check for alternate stack.yaml files?
    !virtualFile.isDirectory && virtualFile.getName == "stack.yaml"
  }

  override def linkAndRefreshProject(
    projectDirectory: String,
    project: Project
  ): Unit = {
    StackProjectImporter.importProject(project, projectDirectory)
  }
}
