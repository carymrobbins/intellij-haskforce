package com.haskforce.codeInsight

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.psi.PsiFile

import com.haskforce.highlighting.annotation.external.{GhcMod, GhcModi}
import com.haskforce.utils.ExecUtil

object LanguageExtensionsProviderFactory {
  def get(psiFile: PsiFile): Option[LanguageExtensionsProvider] = {
    GhcModiLanguageExtensionsProvider.create(psiFile).orElse(
      GhcModLanguageExtensionsProvider.create(psiFile)
    )
  }
}

trait LanguageExtensionsProvider {
  def getLanguages: Array[String]
}

class GhcModiLanguageExtensionsProvider(
  ghcModi: GhcModi
) extends LanguageExtensionsProvider {

  override def getLanguages: Array[String] = {
    Option(ghcModi.syncLang()).getOrElse(Array.empty)
  }
}

object GhcModiLanguageExtensionsProvider {
  def create(psiFile: PsiFile): Option[GhcModiLanguageExtensionsProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiLanguageExtensionsProvider(ghcModi)
}

class GhcModLanguageExtensionsProvider(
  module: Module,
  workDir: String
) extends LanguageExtensionsProvider {

  override def getLanguages: Array[String] = {
    Option(GhcMod.lang(module, workDir)).getOrElse(Array.empty)
  }
}

object GhcModLanguageExtensionsProvider {
  def create(psiFile: PsiFile): Option[GhcModLanguageExtensionsProvider] = for {
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    workDir <- Option(ExecUtil.guessWorkDir(module))
  } yield new GhcModLanguageExtensionsProvider(module, workDir)
}
