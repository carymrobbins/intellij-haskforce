package com.haskforce.tools.hlint

import com.haskforce.system.integrations.highlighting.{ProblemsProvider, ProblemsProviderFactory}
import com.intellij.psi.PsiFile

object LintProblemsProviderFactory extends ProblemsProviderFactory {
  override def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    HLintProblemsProvider.create(psiFile)
  }
}
