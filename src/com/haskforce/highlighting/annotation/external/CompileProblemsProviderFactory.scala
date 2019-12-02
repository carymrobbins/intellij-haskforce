package com.haskforce.highlighting.annotation.external

import com.intellij.psi.PsiFile

import com.haskforce.highlighting.annotation.external.impl.{GhcModiCompileProblemsProvider, GhcModCompileProblemsProvider}

object CompileProblemsProviderFactory extends ProblemsProviderFactory {
  def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    GhcModiCompileProblemsProvider.create(psiFile)
      .orElse(GhcModCompileProblemsProvider.create(psiFile))
  }
}
