package com.haskforce.haskell.highlighting.annotation.external.impl

import java.util.concurrent.Future

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

import com.haskforce.haskell.highlighting.annotation.Problems
import com.haskforce.haskell.highlighting.annotation.external.{ProblemsProvider, GhcModi}
import com.haskforce.system.utils.WrappedFuture

class GhcModiCompileProblemsProvider private(
  ghcModi: GhcModi,
  filePath: String
) extends ProblemsProvider {

  override def getProblems: Option[Problems] = {
    Option(ghcModi.syncCheck(filePath))
  }
}

object GhcModiCompileProblemsProvider {
  def create(psiFile: PsiFile): Option[GhcModiCompileProblemsProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
    filePath <- Option(psiFile.getVirtualFile.getCanonicalPath)
  } yield new GhcModiCompileProblemsProvider(ghcModi, filePath)
}

final class GhcModiFutureProblems(
  underlying: Future[Problems], project: Project
) extends WrappedFuture[Option[Problems]] {

  override def get: Option[Problems] = {
    Option(GhcModi.getFuture(project, underlying))
  }
}
