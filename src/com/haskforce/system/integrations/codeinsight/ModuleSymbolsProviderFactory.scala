package com.haskforce.system.integrations.codeinsight

import com.haskforce.haskell.codeInsight.BrowseItem
import com.haskforce.tools.ghcmod.modi.codeInsight.GhcModiModuleSymbolsProvider
import com.intellij.psi.PsiFile

//TODO document
object ModuleSymbolsProviderFactory {
  def get(psiFile: PsiFile): Option[ModuleSymbolsProvider] = {
    GhcModiModuleSymbolsProvider.create(psiFile)
  }
}

trait ModuleSymbolsProvider {
  def getSymbols(haskellModuleName: String): Array[BrowseItem]
}
