package com.haskforce.tools.ghcmod.mod.codeInsight

import com.haskforce.system.integrations.codeinsight.VisibleModulesProvider
import com.haskforce.system.utils.ExecUtil
import com.haskforce.tools.ghcmod.mod.GhcMod
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.psi.PsiFile

class GhcModVisibleModulesProvider(module: Module, workDir: String) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    Option(GhcMod.list(module, workDir)).getOrElse(Array.empty)
  }
}

object GhcModVisibleModulesProvider {
  def create(psiFile: PsiFile): Option[GhcModVisibleModulesProvider] = for {
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    workDir <- Option(ExecUtil.guessWorkDir(module))
  } yield new GhcModVisibleModulesProvider(module, workDir)
}
