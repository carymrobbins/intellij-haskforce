package com.haskforce.codeInsight

import com.intellij.psi.PsiFile

import com.haskforce.highlighting.annotation.external.GhcModi

object ModuleSymbolsProviderFactory {
  def get(psiFile: PsiFile): Option[ModuleSymbolsProvider] = {
    GhcModiModuleSymbolsProvider.create(psiFile)
  }
}

trait ModuleSymbolsProvider {
  def getSymbols(haskellModuleName: String): Array[BrowseItem]
}

class GhcModiModuleSymbolsProvider(
  ghcModi: GhcModi
) extends ModuleSymbolsProvider {

  override def getSymbols(haskellModuleName: String): Array[BrowseItem] = {
    ghcModi.unsafeBrowse(haskellModuleName)
  }
}

object GhcModiModuleSymbolsProvider {
  def create(psiFile: PsiFile): Option[GhcModiModuleSymbolsProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiModuleSymbolsProvider(ghcModi)
}
