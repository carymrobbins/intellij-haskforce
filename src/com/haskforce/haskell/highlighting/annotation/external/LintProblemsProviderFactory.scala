package com.haskforce.haskell.highlighting.annotation.external

import com.haskforce.tools.hlint.HLintProblemsProvider
import com.intellij.psi.PsiFile

object LintProblemsProviderFactory extends ProblemsProviderFactory {
  override def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    HLintProblemsProvider.create(psiFile)
  }
}
