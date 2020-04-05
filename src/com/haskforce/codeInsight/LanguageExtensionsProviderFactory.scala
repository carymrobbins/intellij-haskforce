package com.haskforce.codeInsight

import com.haskforce.constants.GhcLanguageExtensions
import com.intellij.psi.PsiFile

object LanguageExtensionsProviderFactory {
  def get(psiFile: PsiFile): Option[LanguageExtensionsProvider] = {
    cached
  }

  private val cached = Some(GhcLanguageExtensionsProvider)
}

trait LanguageExtensionsProvider {
  def getLanguages: Array[String]
}

object GhcLanguageExtensionsProvider extends LanguageExtensionsProvider {
  override def getLanguages: Array[String] = GhcLanguageExtensions.stringArray
}
