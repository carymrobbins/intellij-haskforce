package com.haskforce.highlighting.annotation.external

import com.intellij.psi.PsiFile
import com.haskforce.highlighting.annotation.external.impl.{GhcModCompileProblemsProvider, GhcModiCompileProblemsProvider, HsDevCompileProblemsProvider}

object CompileProblemsProviderFactory extends ProblemsProviderFactory {
  def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    HsDevCompileProblemsProvider.create(psiFile)
      .orElse(GhcModiCompileProblemsProvider.create(psiFile))
      .orElse(GhcModCompileProblemsProvider.create(psiFile))
  }
}
