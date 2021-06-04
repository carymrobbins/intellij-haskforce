package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.projectImport.ProjectOpenProcessor

// TODO: This class seems to obviate the need for 'StackOpenProjectProvider'?
class StackProjectOpenProcessor extends ProjectOpenProcessor {

  override def getName: String = "Haskell Stack"

  override def canOpenProject(file: VirtualFile): Boolean = {
    new StackOpenProjectProvider().canOpenProject(file)
  }

  override def doOpenProject(
    virtualFile: VirtualFile,
    projectToClose: Project,
    forceOpenInNewFrame: Boolean
  ): Project = {
    new StackOpenProjectProvider().openProject(
      virtualFile,
      projectToClose,
      forceOpenInNewFrame
    )
  }
}
