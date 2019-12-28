package com.haskforce.highlighting.annotation.external.impl

import com.haskforce.highlighting.annotation.Problems
import com.haskforce.highlighting.annotation.external.{GhcModi, ProblemsProvider}
import com.intellij.psi.PsiFile

class GhcModiCompileProblemsProvider private(
  ghcModi: GhcModi,
  psiFile: PsiFile
) extends ProblemsProvider {

  override def requiresFileSave: Boolean = false

  override def getProblems: Option[Problems] = {
    Option(ghcModi.check(psiFile))
  }
}

object GhcModiCompileProblemsProvider {
  def create(psiFile: PsiFile): Option[GhcModiCompileProblemsProvider] =
    GhcModi.get(psiFile).map(new GhcModiCompileProblemsProvider(_, psiFile))
}
