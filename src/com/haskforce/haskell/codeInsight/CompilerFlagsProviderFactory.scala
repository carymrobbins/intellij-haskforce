package com.haskforce.haskell.codeInsight

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.psi.PsiFile

import com.haskforce.haskell.constants.GhcFlags
import com.haskforce.haskell.highlighting.annotation.external.{GhcMod, GhcModi}
import com.haskforce.utils.ExecUtil

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
