package com.haskforce.highlighting.annotation.external.impl

import java.util.concurrent.{ExecutionException, Executors, Future}

import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

import com.haskforce.Implicits._
import com.haskforce.highlighting.annotation.Problems
import com.haskforce.highlighting.annotation.external.{ProblemsProvider, HLint, WrappedFuture}
import com.haskforce.utils.{NotificationUtil, ExecUtil}

class HLintProblemsProvider private(
  project: Project,
  workDir: String,
  filePath: String
) extends ProblemsProvider {

  override def getProblems: WrappedFuture[Option[Problems]] = {
    new HLintFutureProblems(
      project,
      HLintProblemsProvider.executorService.submit({ () =>
        HLint.lint(project, workDir, filePath)
      }.toCallable)
    )
  }
}

object HLintProblemsProvider {
  def create(psiFile: PsiFile): Option[HLintProblemsProvider] = for {
    filePath <- Option(psiFile.getVirtualFile.getCanonicalPath)
    workDir = ExecUtil.guessWorkDir(psiFile)
    project = psiFile.getProject
  } yield new HLintProblemsProvider(project, workDir, filePath)

  val executorService = Executors.newSingleThreadExecutor()

  val LOG = Logger.getInstance(classOf[HLintProblemsProvider])
}

class HLintFutureProblems(
  project: Project,
  underlying: Future[Problems]
) extends WrappedFuture[Option[Problems]] {

  override def get: Option[Problems] = {
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
