package com.haskforce.haskell.highlighting.annotation.external

import com.intellij.psi.PsiFile

import com.haskforce.haskell.highlighting.annotation.external.impl.HLintProblemsProvider

object LintProblemsProviderFactory extends ProblemsProviderFactory {
  override def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    HLintProblemsProvider.create(psiFile)
  }
}
