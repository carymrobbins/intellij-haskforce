package com.haskforce.tools.ghcmod.modi.highlighting

import java.util.concurrent.Future

import com.haskforce.system.integrations.highlighting.{HaskellProblem, ProblemsProvider}
import com.haskforce.system.utils.WrappedFuture
import com.haskforce.tools.ghcmod.modi.GhcModi
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

class GhcModiCompileProblemsProvider private(
  ghcModi: GhcModi,
  filePath: String
) extends ProblemsProvider {

  override def getProblems: Option[java.util.List[HaskellProblem]] = {
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
  underlying: Future[java.util.List[HaskellProblem]], project: Project
) extends WrappedFuture[Option[java.util.List[HaskellProblem]]] {

  override def get: Option[java.util.List[HaskellProblem]] = {
    Option(GhcModi.getFuture(project, underlying))
  }
}
