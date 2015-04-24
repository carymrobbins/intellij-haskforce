package com.haskforce.utils

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiDirectory, PsiDocumentManager, PsiFile}
import com.intellij.util.Function
import org.jetbrains.annotations.{NotNull, Nullable}

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.language.implicitConversions

object FileUtil {
  implicit def funToRunnable(block: => Unit): Runnable = new Runnable { def run() = block }

  def updateFileText(project: Project, file: PsiFile, function: Function[String, String]) {
    val app = ApplicationManager.getApplication
    app.saveAll()
    app.invokeLater {
      Option(PsiDocumentManager.getInstance(project).getDocument(file)).foreach { document =>
        CommandProcessor.getInstance.executeCommand(project, {
          app.runWriteAction {
            document.setText(function.fun(document.getText))
          }
        }, "Update text for " + file.getName, "", document)
      }
    }
  }

  /**
   * Returns an array of directory names as the relative path to `file` from the source root.
   * For example, given file "project/src/foo/bar/baz.hs" the result would be `{"foo", "bar"}`.
   */
  @Nullable
  def getPathFromSourceRoot(project: Project, file: VirtualFile): java.util.List[String] = {
    @tailrec
    def loop(file: VirtualFile, rootPath: String, initial: List[String] = List()): List[String] = {
      if (rootPath == file.getCanonicalPath) initial
      else loop(file.getParent, rootPath, file.getName :: initial)
    }
    val result = for {
      root <- Option(getSourceRoot(project, file))
      rootPath <- Option(root.getCanonicalPath)
    } yield loop(file, rootPath, List())
    // TODO: This could return Option instead.
    result.getOrElse(null)
  }

  @Nullable
  def getSourceRoot(project: Project, file: VirtualFile): VirtualFile = {
    @tailrec
    def loop(file: VirtualFile, rootPath: String): VirtualFile = {
      if (rootPath == file.getCanonicalPath) file
      else loop(file.getParent, rootPath)
    }
    val result = for {
      project <- Option(project)
      file <- Option(file)
    } yield ProjectRootManager.getInstance(project).getContentSourceRoots.view.map { root =>
      for {
        root <- Option(root)
        rootPath <- Option(root.getCanonicalPath)
      } yield loop(file, rootPath)
    }.headOption
    // TODO: This could return Option instead.
    result.flatten.flatten.orNull
  }

  def findOrCreateSubdirectory(@NotNull dir: PsiDirectory, @NotNull name: String): PsiDirectory = {
    Option(dir.findSubdirectory(name)).getOrElse(dir.createSubdirectory(name))
  }
}
