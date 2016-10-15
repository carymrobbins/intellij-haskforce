package com.haskforce.haskell.codeInsight

import com.haskforce.haskell.constants.GhcFlags
import com.intellij.psi.PsiFile

object CompilerFlagsProviderFactory {
  def get(psiFile: PsiFile): Option[CompilerFlagsProvider] = {
    Some(GhcCompilerFlagsProvider)
  }
}

trait CompilerFlagsProvider {
  def getFlags: Array[String]
}

object GhcCompilerFlagsProvider extends CompilerFlagsProvider {
  override def getFlags: Array[String] = GhcFlags.list
}
