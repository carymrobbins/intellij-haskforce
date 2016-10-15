package com.haskforce.tools.ghcmod.mod.highlighting

import java.util.concurrent.{ExecutionException, Executors, Future}

import com.haskforce.system.integrations.highlighting.{HaskellProblem, ProblemsProvider}
import com.haskforce.system.utils.{ExecUtil, NotificationUtil, WrappedFuture}
import com.haskforce.tools.ghcmod.mod.GhcMod
import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

class GhcModCompileProblemsProvider private(
  module: Module,
  workDir: String,
  filePath: String
) extends ProblemsProvider {

  override def getProblems: Option[java.util.List[HaskellProblem]] = {
    Option(GhcMod.check(module, workDir, filePath))
  }
}

object GhcModCompileProblemsProvider {
  def create(psiFile: PsiFile): Option[GhcModCompileProblemsProvider] = for {
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    filePath <- Option(psiFile.getVirtualFile.getCanonicalPath)
    workDir = ExecUtil.guessWorkDir(psiFile)
  } yield new GhcModCompileProblemsProvider(module, workDir, filePath)

  val executorService = Executors.newSingleThreadExecutor()

  val LOG = Logger.getInstance(classOf[GhcModCompileProblemsProvider])
}

class GhcModFutureProblems(
  project: Project,
  underlying: Future[List[HaskellProblem]]
) extends WrappedFuture[Option[List[HaskellProblem]]] {

  override def get: Option[List[HaskellProblem]] = {
    try {
      Option(underlying.get())
    } catch {
      case e: InterruptedException =>
        displayError("Interrupted", e)
        None
      case e: ExecutionException =>
        displayError("Execution aborted", e)
        None
    }
  }

  private def displayError(prefix: String, e: Exception): Unit = {
    GhcModCompileProblemsProvider.LOG.error(prefix, e)
    NotificationUtil.displayToolsNotification(
      NotificationType.ERROR, project, "ghc-mod", s"$prefix: $e"
    )
  }
}


