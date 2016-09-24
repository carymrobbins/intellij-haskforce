package com.haskforce.system.integrations.codeinsight

import com.haskforce.tools.ghcmod.mod.codeInsight.GhcModVisibleModulesProvider
import com.haskforce.tools.ghcmod.modi.codeInsight.GhcModiVisibleModulesProvider
import com.intellij.psi.PsiFile

//TODO document
object VisibleModulesProviderFactory {
  def get(psiFile: PsiFile): Option[VisibleModulesProvider] = {
    GhcModiVisibleModulesProvider.create(psiFile).orElse(
      GhcModVisibleModulesProvider.create(psiFile)
    )
  }
}

trait VisibleModulesProvider {
  def getVisibleModules: Array[String]
}
