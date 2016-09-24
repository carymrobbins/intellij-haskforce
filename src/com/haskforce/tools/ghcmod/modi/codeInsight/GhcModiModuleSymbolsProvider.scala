package com.haskforce.tools.ghcmod.modi.codeInsight

import com.haskforce.haskell.codeInsight.BrowseItem
import com.haskforce.system.integrations.codeinsight.ModuleSymbolsProvider
import com.haskforce.tools.ghcmod.modi.GhcModi
import com.intellij.psi.PsiFile


class GhcModiModuleSymbolsProvider(ghcModi: GhcModi) extends ModuleSymbolsProvider {

  override def getSymbols(haskellModuleName: String): Array[BrowseItem] = {
    Option(ghcModi.syncBrowse(haskellModuleName)).getOrElse(Array.empty)
  }
}

object GhcModiModuleSymbolsProvider {
  def create(psiFile: PsiFile): Option[GhcModiModuleSymbolsProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiModuleSymbolsProvider(ghcModi)
}
