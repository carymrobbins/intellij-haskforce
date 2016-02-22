package com.haskforce.highlighting.annotation.external.impl

import java.util.concurrent.Future

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

import com.haskforce.highlighting.annotation.Problems
import com.haskforce.highlighting.annotation.external.{ProblemsProvider, GhcModi}
import com.haskforce.utils.WrappedFuture

class GhcModiCompileProblemsProvider private(
  module: Module,
  ghcModi: GhcModi,
  filePath: String
) extends ProblemsProvider {

  override def getProblems: WrappedFuture[Option[Problems]] = {
    new GhcModiFutureProblems(ghcModi.check(filePath), module.getProject)
  }
}

object GhcModiCompileProblemsProvider {
  def create(psiFile: PsiFile): Option[GhcModiCompileProblemsProvider] = for {
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    ghcModi <- Option(module.getComponent(classOf[GhcModi])) if ghcModi.isConfigured
    filePath <- Option(psiFile.getVirtualFile.getCanonicalPath)
  } yield new GhcModiCompileProblemsProvider(module, ghcModi, filePath)
}

final class GhcModiFutureProblems(
  underlying: Future[Problems], project: Project
) extends WrappedFuture[Option[Problems]] {

  override def get: Option[Problems] = {
    Option(GhcModi.getFuture(project, underlying))
  }
}
