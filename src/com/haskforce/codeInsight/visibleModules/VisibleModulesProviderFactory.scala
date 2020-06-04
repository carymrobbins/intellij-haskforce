package com.haskforce.codeInsight.visibleModules

import com.intellij.psi.PsiFile

object VisibleModulesProviderFactory {
  def get(psiFile: PsiFile): Option[VisibleModulesProvider] = {
    providers.toStream.flatMap(_(psiFile).toStream).headOption
  }

  // Providers are checked for in this order; first configured provider wins.
  private val providers = List(
    GhcModiVisibleModulesProvider.create(_),
    GhcModVisibleModulesProvider.create(_),
    GhcPkgDumpVisibleModulesProvider.create(_)
  )
}
