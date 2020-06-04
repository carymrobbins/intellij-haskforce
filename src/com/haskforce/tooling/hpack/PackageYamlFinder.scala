package com.haskforce.tooling.hpack

import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.module.ModuleUtilCore
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiFile, PsiManager}

object PackageYamlFinder {

  def virtualForFile(psiFile: PsiFile): Option[VirtualFile] = {
    for {
      module <- Option(ModuleUtilCore.findModuleForFile(psiFile))
      moduleFile <- Option(module.getModuleFile)
      moduleDir <- Option(moduleFile.getParent)
      vFile <- Option(moduleDir.findChild("package.yaml"))
    } yield vFile
  }

  def psiForFile(psiFile: PsiFile): Option[PsiFile] = {
    virtualForFile(psiFile).flatMap { vFile =>
      val psiManager = PsiManager.getInstance(psiFile.getProject)
      ReadAction.compute(() => Option(psiManager.findFile(vFile)))
    }
  }
}
