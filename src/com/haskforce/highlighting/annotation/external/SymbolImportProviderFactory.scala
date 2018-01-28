package com.haskforce.highlighting.annotation.external

import com.haskforce.highlighting.annotation.external.impl.GhcModiSymbolImportProvider
import com.intellij.psi.PsiFile

object SymbolImportProviderFactory {
  def get(psiFile: PsiFile): Option[SymbolImportProvider] = {
    GhcModiSymbolImportProvider.create(psiFile)
  }
}
