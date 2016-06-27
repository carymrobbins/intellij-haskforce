package com.haskforce.highlighting.annotation.external.impl

import java.util.concurrent.{ExecutionException, Executors, Future}

import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

import com.haskforce.highlighting.annotation.Problems
import com.haskforce.highlighting.annotation.external.{HLint, ProblemsProvider}
import com.haskforce.psi.HaskellFile
import com.haskforce.utils.parser.CastUtil
import com.haskforce.utils.{ExecUtil, NotificationUtil, WrappedFuture}

class HLintProblemsProvider private(
  project: Project,
  workDir: String,
  filePath: String,
  haskellFile: HaskellFile
) extends ProblemsProvider {

  override def getProblems: Option[Problems] = {
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
