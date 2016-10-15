package com.haskforce.system.integrations.codeinsight

import com.haskforce.haskell.codeInsight.BrowseItem
import com.haskforce.tools.ghcmod.modi.codeInsight.GhcModiModuleSymbolsProvider
import com.intellij.psi.PsiFile

/**
  * Factory for getting the ModuleSymbolsProvider for the PsiFile
  */
object ModuleSymbolsProviderFactory {
  def get(psiFile: PsiFile): Option[ModuleSymbolsProvider] = {
    GhcModiModuleSymbolsProvider.create(psiFile)
  }
}

/**
  * Used to obtain the Symbols for a Haskell-Module, used for Code-Completion
  */
trait ModuleSymbolsProvider {
  /**
    * returns the Symbols the for Haskell-Module
    * @param haskellModuleName the Module-Name
    * @return a list of BrowseItems
    */
  def getSymbols(haskellModuleName: String): Array[BrowseItem]
}
