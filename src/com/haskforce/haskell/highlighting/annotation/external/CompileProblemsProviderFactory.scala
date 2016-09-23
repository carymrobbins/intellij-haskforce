package com.haskforce.haskell.highlighting.annotation.external

import com.intellij.psi.PsiFile
import com.haskforce.haskell.highlighting.annotation.external.impl.{GhcModCompileProblemsProvider, GhcModiCompileProblemsProvider}
import com.haskforce.system.integrations.highlighting.{ProblemsProvider, ProblemsProviderFactory}

object CompileProblemsProviderFactory extends ProblemsProviderFactory {
  def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    GhcModiCompileProblemsProvider.create(psiFile).orElse(
      GhcModCompileProblemsProvider.create(psiFile)
    )
  }
}
