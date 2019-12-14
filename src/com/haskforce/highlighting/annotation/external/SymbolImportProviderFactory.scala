package com.haskforce.highlighting.annotation.external

import com.haskforce.highlighting.annotation.external.impl.{GhcModiSymbolImportProvider, HsDevSymbolImportProvider}
import com.intellij.psi.PsiFile

object SymbolImportProviderFactory {
  def get(psiFile: PsiFile): Option[SymbolImportProvider] = {
    HsDevSymbolImportProvider.create(psiFile)
      .orElse(GhcModiSymbolImportProvider.create(psiFile))
  }
}
