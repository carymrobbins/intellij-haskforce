package com.haskforce.system.utils

import java.io.File
import java.util

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.language.implicitConversions

import com.haskforce.Implicits._
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiDirectory, PsiDocumentManager, PsiFile}
import com.intellij.util.Function
import org.jetbrains.annotations.{NotNull, Nullable}

object FileUtil {
  def updateFileText(project: Project, file: PsiFile, function: Function[String, String]) {
    val app = ApplicationManager.getApplication
    app.saveAll()
    app.invokeLater { () =>
      Option(PsiDocumentManager.getInstance(project).getDocument(file)).foreach { document =>
        CommandProcessor.getInstance.executeCommand(project, { () =>
          app.runWriteAction({ () =>
            document.setText(function.fun(document.getText))
          }: Runnable)
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
    result.map(seqAsJavaList).orNull
  }

  @Nullable
  def getSourceRoot(project: Project, file: VirtualFile): VirtualFile = {
    @tailrec
    def loop(maybeFile: Option[VirtualFile], rootPath: String): Option[VirtualFile] = {
      maybeFile match {
        case None => None
        case Some(f) => if (rootPath == f.getCanonicalPath) Some(f) else loop(Option(f.getParent), rootPath)
      }
    }
    val result = for {
      project <- Option(project)
      file <- Option(file)
    } yield ProjectRootManager.getInstance(project).getContentSourceRoots.view.map { root =>
      for {
        root <- Option(root)
        rootPath <- Option(root.getCanonicalPath)
      } yield loop(Some(file), rootPath)
    }.headOption
    // TODO: This could return Option instead.
    result.flatten.flatten.flatten.orNull
  }

  def findOrCreateSubdirectory(@NotNull dir: PsiDirectory, @NotNull name: String): PsiDirectory = {
    Option(dir.findSubdirectory(name)).getOrElse(dir.createSubdirectory(name))
  }

  def join(base: String, paths: String*): String = {
    paths.foldLeft(base) { (acc, path) => new File(acc, path.replaceFirst("^\\.(\\\\|/)", "")).getPath }
  }

  def findFilesRecursively(dir: VirtualFile, p: VirtualFile => Boolean): Seq[VirtualFile] = {
    FileTreeLike.findRecursively(dir, p)
  }

  def findFilesRecursively(file: File): util.Collection[File] = {
    FileTreeLike.findRecursively(file, (_: File) => true).asJavaCollection
  }

  /**
   * Returns the path of absPath relative to basePath.
   */
  def toRelativePath(basePath: String, absPath: String): String = {
    new File(basePath).toURI.relativize(new File(absPath).toURI).getPath match {
      case "" => "." + File.separator
      case s => s
    }
  }

  /**
   * Builds a relative path to file from the project root.  If the project root is null, returns the canonical path.
   */
  def toRelativePath(project: Project, file: VirtualFile): String = {
    val path = file.getCanonicalPath
    Option(project.getBasePath).map(toRelativePath(_, path)).getOrElse(path)
  }
}

trait FileTreeLike[A] {
  def childrenOf(a: A): Stream[A]
}

object FileTreeLike {

  def findRecursively[A : FileTreeLike](dir: A, p: A => Boolean): Stream[A] = {
    @tailrec
    def loop(files: Stream[A], acc: Stream[A]): Stream[A] = files match {
      case Stream.Empty => acc
      case f #:: rest =>
        val children = FileTreeLike.childrenOf(f)
        val newAcc = if (p(f)) f +: acc else acc
        loop(children #::: rest, newAcc)
    }
    loop(Stream(dir), Stream.Empty)
  }

  def childrenOf[A : FileTreeLike](a: A): Stream[A] = implicitly[FileTreeLike[A]].childrenOf(a)

  implicit val javaFile: FileTreeLike[File] = new FileTreeLike[File] {
    override def childrenOf(a: File): Stream[File] = {
      NullUtil.fold(a.listFiles())(Stream.empty, _.toStream)
    }
  }

  implicit val virtualFile: FileTreeLike[VirtualFile] = new FileTreeLike[VirtualFile] {
    override def childrenOf(a: VirtualFile): Stream[VirtualFile] = {
      NullUtil.fold(a.getChildren)(Stream.empty, _.toStream)
    }
  }
}
