package com.haskforce.codeInsight

import com.intellij.psi.PsiFile

import com.haskforce.highlighting.annotation.external.GhcModi
import com.haskforce.highlighting.annotation.external.hsdev.HsDevExecutor

object ModuleSymbolsProviderFactory {
  def get(psiFile: PsiFile): Option[ModuleSymbolsProvider] = {
    HsDevModuleSymbolsProvider.create(psiFile)
      .orElse(GhcModiModuleSymbolsProvider.create(psiFile))
  }
}

trait ModuleSymbolsProvider {
  def getSymbols(haskellModuleName: String): Array[BrowseItem]
}

class GhcModiModuleSymbolsProvider(
  ghcModi: GhcModi
) extends ModuleSymbolsProvider {

  override def getSymbols(haskellModuleName: String): Array[BrowseItem] = {
    Option(ghcModi.syncBrowse(haskellModuleName)).getOrElse(Array.empty)
  }
}

object GhcModiModuleSymbolsProvider {
  def create(psiFile: PsiFile): Option[GhcModiModuleSymbolsProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiModuleSymbolsProvider(ghcModi)
}

class HsDevModuleSymbolsProvider(
  hsdev: HsDevExecutor
) extends ModuleSymbolsProvider {

  override def getSymbols(haskellModuleName: String): Array[BrowseItem] = {
    hsdev.installedModulesMap.get(haskellModuleName) match {
      case None => Array.empty
      case Some(m) =>
        m.exports.iterator.map(e =>
          BrowseItem(e.id.name, e.id.module.name, e.info.`type`)
        ).toArray
    }
  }
}

object HsDevModuleSymbolsProvider {
  def create(psiFile: PsiFile): Option[HsDevModuleSymbolsProvider] = {
    HsDevExecutor.get(psiFile).map(new HsDevModuleSymbolsProvider(_))
  }
}
