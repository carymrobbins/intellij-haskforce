package com.haskforce.system.integrations.codeinsight

import com.haskforce.tools.ghcmod.mod.codeInsight.GhcModVisibleModulesProvider
import com.haskforce.tools.ghcmod.modi.codeInsight.GhcModiVisibleModulesProvider
import com.intellij.psi.PsiFile


/**
  * Factory for getting the VisibleModulesProvider for the PsiFile
  */
object VisibleModulesProviderFactory {
  def get(psiFile: PsiFile): Option[VisibleModulesProvider] = {
    GhcModiVisibleModulesProvider.create(psiFile).orElse(
      GhcModVisibleModulesProvider.create(psiFile)
    )
  }
}

/**
  * returns the currently visible Modules for the CodeCompletion
  */
trait VisibleModulesProvider {
  /**
    * the currently visible Modules for this PsiFile
    */
  def getVisibleModules: Array[String]
}
