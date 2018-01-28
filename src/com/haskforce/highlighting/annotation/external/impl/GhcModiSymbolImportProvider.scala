package com.haskforce.highlighting.annotation.external.impl

import com.haskforce.highlighting.annotation.external.{GhcModi, SymbolImportProvider}
import com.intellij.psi.PsiFile

class GhcModiSymbolImportProvider private(
  ghcModi: GhcModi,
  filePath: String
) extends SymbolImportProvider {
  override def findImport(symbol: String): Seq[SymbolImportProvider.Result] =
    ghcModi.find(symbol).toSeq.map(
      importText => SymbolImportProvider.Result(importText, symbol)
    )
}

object GhcModiSymbolImportProvider {
  def create(psiFile: PsiFile): Option[GhcModiSymbolImportProvider] = for {
    ghcModi  <- GhcModi.get(psiFile)
    filePath <- Option(psiFile.getVirtualFile.getCanonicalPath)
  } yield new GhcModiSymbolImportProvider(ghcModi, filePath)
}
