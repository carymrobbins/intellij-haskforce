package com.haskforce.haskell.codeInsight

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.psi.PsiFile

import com.haskforce.haskell.constants.GhcLanguageExtensions
import com.haskforce.haskell.highlighting.annotation.external.{GhcMod, GhcModi}
import com.haskforce.utils.ExecUtil

object LanguageExtensionsProviderFactory {
  def get(psiFile: PsiFile): Option[LanguageExtensionsProvider] = {
    Some(GhcLanguageExtensionsProvider)
  }
}

trait LanguageExtensionsProvider {
  def getLanguages: Array[String]
}

object GhcLanguageExtensionsProvider extends LanguageExtensionsProvider {
  override def getLanguages: Array[String] = GhcLanguageExtensions.stringArray
}
