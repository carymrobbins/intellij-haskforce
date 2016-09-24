package com.haskforce.tools.ghcmod.modi.highlighting

import com.haskforce.system.integrations.highlighting.{ProblemsProvider, ProblemsProviderFactory}
import com.haskforce.tools.ghcmod.mod.highlighting.GhcModCompileProblemsProvider
import com.intellij.psi.PsiFile

object CompileProblemsProviderFactory extends ProblemsProviderFactory {
  def get(psiFile: PsiFile): Option[ProblemsProvider] = {
    GhcModiCompileProblemsProvider.create(psiFile).orElse(
      GhcModCompileProblemsProvider.create(psiFile)
    )
  }
}
