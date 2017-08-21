package com.haskforce.cabal.completion

import prelude._

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.haskforce.cabal.lang.psi.CabalFile
import com.haskforce.psi.HaskellFile
import com.haskforce.utils.IJReadAction

/** Utility for finding Cabal files. */
object CabalFileFinder {

  def virtualForModule(module: Module): Option[VirtualFile] = {
    for {
      parent <- SModule(module).getModuleFile.getParent
      children <- parent.getChildren
      result <- children.find(_.getExtension.contains("cabal"))
    } yield result.toVirtualFile
  }

  def virtualForFile(file: HaskellFile): Option[VirtualFile] = {
    Option(ModuleUtilCore.findModuleForPsiElement(file)).flatMap(virtualForModule)
  }

  def psiForFile(file: HaskellFile): IJReadAction[Option[CabalFile]] = {
    virtualForFile(file).traverse(getPsi(file.getProject, _)).map(_.flatten)
  }

  private def getPsi(project: Project, cabalVFile: VirtualFile): IJReadAction[Option[CabalFile]] = {
    SPsiManager.getInstance(project).findFile(cabalVFile).map(_.map(_.toPsiFile match {
      case cabalPsiFile: CabalFile => cabalPsiFile
      case other => throw new AssertionError(s"Expected CabalFile, got: $other")
    }))
  }
}
