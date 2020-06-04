package com.haskforce.codeInsight.visibleModules

import com.haskforce.highlighting.annotation.external.GhcMod
import com.haskforce.utils.ExecUtil
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.psi.PsiFile

class GhcModVisibleModulesProvider(
  module: Module,
  workDir: String
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    Option(GhcMod.list(module, workDir)).getOrElse(Array.empty)
  }
}

object GhcModVisibleModulesProvider {
  def create(psiFile: PsiFile): Option[GhcModVisibleModulesProvider] = for {
    // Guard against ghc-mod not being configured.
    _ <- Option(GhcMod.getPath(psiFile.getProject))
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    workDir <- Option(ExecUtil.guessWorkDir(module))
  } yield new GhcModVisibleModulesProvider(module, workDir)
}
