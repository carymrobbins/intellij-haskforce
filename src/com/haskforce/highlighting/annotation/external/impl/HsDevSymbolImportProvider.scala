package com.haskforce.highlighting.annotation.external.impl

import com.haskforce.highlighting.annotation.external.SymbolImportProvider
import com.haskforce.highlighting.annotation.external.hsdev.HsDevExecutor
import com.intellij.psi.PsiFile

class HsDevSymbolImportProvider private(
  hsdev: HsDevExecutor
) extends SymbolImportProvider {
  override def findImport(symbol: String): Seq[SymbolImportProvider.Result] = {
    hsdev.symbols
      .getOrElse(symbol, Vector.empty)
      .map(s =>
        SymbolImportProvider.Result(
          importText = s.id.module.name,
          symbolText = s.id.name
        )
      )
      .distinct
      .sortBy(_.importText)
  }
}

object HsDevSymbolImportProvider {
  def create(psiFile: PsiFile): Option[HsDevSymbolImportProvider] = {
    HsDevExecutor.get(psiFile).map(new HsDevSymbolImportProvider(_))
  }
}
