package com.haskforce.tools.hlint

import com.haskforce.haskell.highlighting.annotation.external.{ProblemsProvider, ProblemsProviderFactory}
import com.intellij.psi.PsiFile

object LintProblemsProviderFactory extends ProblemsProviderFactory {
  override def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    HLintProblemsProvider.create(psiFile)
  }
}
