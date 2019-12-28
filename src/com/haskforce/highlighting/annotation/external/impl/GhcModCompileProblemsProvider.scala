package com.haskforce.highlighting.annotation.external.impl

import java.util.concurrent.{ExecutionException, Executors, Future}

import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

import com.haskforce.Implicits._
import com.haskforce.highlighting.annotation.Problems
import com.haskforce.highlighting.annotation.external.{ProblemsProvider, GhcMod}
import com.haskforce.utils.{WrappedFuture, NotificationUtil, ExecUtil}

class GhcModCompileProblemsProvider private(
  module: Module,
  workDir: String,
  filePath: String
) extends ProblemsProvider {

  override def requiresFileSave: Boolean = true

  override def getProblems: Option[Problems] = {
    Option(GhcMod.check(module, workDir, filePath))
  }
}

object GhcModCompileProblemsProvider {
  def create(psiFile: PsiFile): Option[GhcModCompileProblemsProvider] = for {
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    filePath <- Option(psiFile.getVirtualFile.getCanonicalPath)
    workDir = ExecUtil.guessWorkDir(psiFile)
  } yield new GhcModCompileProblemsProvider(module, workDir, filePath)

  val LOG = Logger.getInstance(classOf[GhcModCompileProblemsProvider])
}

class GhcModFutureProblems(
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
    GhcModCompileProblemsProvider.LOG.error(prefix, e)
    NotificationUtil.displayToolsNotification(
      NotificationType.ERROR, project, "ghc-mod", s"$prefix: $e"
    )
  }
}


