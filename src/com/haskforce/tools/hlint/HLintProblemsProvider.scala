package com.haskforce.tools.hlint

import java.util.concurrent.{ExecutionException, Executors, Future}

import com.haskforce.haskell.psi.HaskellFile
import com.haskforce.system.integrations.highlighting.{HaskellProblem, ProblemsProvider}
import com.haskforce.system.utils.parser.CastUtil
import com.haskforce.system.utils.{ExecUtil, NotificationUtil, WrappedFuture}
import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

class HLintProblemsProvider private(
  project: Project,
  workDir: String,
  filePath: String,
  haskellFile: HaskellFile
) extends ProblemsProvider {

  override def getProblems: Option[java.util.List[HaskellProblem]] = {
    Option(HLint.lint(project, workDir, filePath, haskellFile))
  }
}

object HLintProblemsProvider {
  def create(psiFile: PsiFile): Option[HLintProblemsProvider] = for {
    haskellFile <- CastUtil.down[HaskellFile](psiFile)
    filePath <- Option(psiFile.getVirtualFile.getCanonicalPath)
    workDir = ExecUtil.guessWorkDir(psiFile)
    project = psiFile.getProject
  } yield new HLintProblemsProvider(project, workDir, filePath, haskellFile)

  val executorService = Executors.newSingleThreadExecutor()

  val LOG = Logger.getInstance(classOf[HLintProblemsProvider])
}

class HLintFutureProblems(
  project: Project,
  underlying: Future[java.util.List[HaskellProblem]]
) extends WrappedFuture[Option[java.util.List[HaskellProblem]]] {

  override def get: Option[java.util.List[HaskellProblem]] = {
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
    HLintProblemsProvider.LOG.error(prefix, e)
    NotificationUtil.displayToolsNotification(
      NotificationType.ERROR, project, "hlint", s"$prefix: $e"
    )
  }
}
