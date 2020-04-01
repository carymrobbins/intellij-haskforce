package com.haskforce.codeInsight

import com.haskforce.constants.GhcFlags
import com.intellij.psi.PsiFile

object CompilerFlagsProviderFactory {
  def get(psiFile: PsiFile): Option[CompilerFlagsProvider] = {
    cached
  }

  private val cached = Some(GhcCompilerFlagsProvider)
}

trait CompilerFlagsProvider {
  def getFlags: Array[String]
}

object GhcCompilerFlagsProvider extends CompilerFlagsProvider {
  override def getFlags: Array[String] = GhcFlags.list
}
