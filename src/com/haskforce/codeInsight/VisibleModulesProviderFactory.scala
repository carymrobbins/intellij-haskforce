package com.haskforce.codeInsight

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.psi.PsiFile
import com.haskforce.highlighting.annotation.external.{GhcMod, GhcModi}
import com.haskforce.highlighting.annotation.external.hsdev.{HsDevExecutor, HsDevModuleLocation}
import com.haskforce.utils.ExecUtil

object VisibleModulesProviderFactory {
  def get(psiFile: PsiFile): Option[VisibleModulesProvider] = {
    HsDevVisibleModulesProvider.create(psiFile)
      .orElse(GhcModiVisibleModulesProvider.create(psiFile))
      .orElse(GhcModVisibleModulesProvider.create(psiFile))
  }
}

trait VisibleModulesProvider {
  def getVisibleModules: Array[String]
}

class GhcModiVisibleModulesProvider(
  ghcModi: GhcModi
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    Option(ghcModi.syncList()).getOrElse(Array.empty)
  }
}

object GhcModiVisibleModulesProvider {
  def create(psiFile: PsiFile): Option[GhcModiVisibleModulesProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiVisibleModulesProvider(ghcModi)
}

class GhcModVisibleModulesProvider(
  module: Module,
  workDir: String
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    Option(GhcMod.list(module, workDir)).getOrElse(Array.empty)
  }
}

object GhcModVisibleModulesProvider {
  def create(psiFile: PsiFile): Option[GhcModVisibleModulesProvider] = for {
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    workDir <- Option(ExecUtil.guessWorkDir(module))
  } yield new GhcModVisibleModulesProvider(module, workDir)
}

class HsDevVisibleModulesProvider(
  hsdev: HsDevExecutor
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    hsdev.installedModules.iterator
      .filter(m => HsDevModuleLocation.exposed(m.id.location).getOrElse(false))
      .map(_.id.name)
      .toArray
  }
}

object HsDevVisibleModulesProvider {
  def create(psiFile: PsiFile): Option[HsDevVisibleModulesProvider] = {
    HsDevExecutor.get(psiFile).map(new HsDevVisibleModulesProvider(_))
  }
}
