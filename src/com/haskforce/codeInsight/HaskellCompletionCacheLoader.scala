package com.haskforce.codeInsight

import java.util

import com.haskforce.psi.HaskellFile
import com.intellij.AppTopics
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.project.{Project, ProjectManagerListener}
import com.intellij.openapi.vfs._
import com.intellij.psi.PsiManager

import scala.annotation.tailrec

/** Loads the completion cache for any Haskell files we have open. */
class HaskellCompletionCacheLoader extends ProjectManagerListener {

  override def projectOpened(project: Project): Unit = {
    project.getMessageBus.connect().subscribe(
      AppTopics.FILE_DOCUMENT_SYNC,
      new HaskellCompletionCacheLoader.Handler(project)
    )
  }
}

object HaskellCompletionCacheLoader {

  def getService(project: Project): HaskellCompletionCacheService = {
    project.getService(classOf[HaskellCompletionCacheService])
  }

  final class Cache {
    val ghcFlags: util.Set[String] = new util.HashSet(300)
    val languageExtensions: util.Set[LookupElementWrapper] = new util.HashSet(300)
    val moduleSymbols: util.Map[String, util.Set[LookupElementWrapper]] = new util.HashMap(10)
  }

  /**
   * Essentially a newtype wrapper so we can override hashCode and equals
   * as comparators for HashSet.
   */
  final class LookupElementWrapper(val get: LookupElement) {
    override def hashCode(): Int = get.getLookupString.hashCode

    @tailrec
    override def equals(obj: Any): Boolean = obj match {
      case o: LookupElementWrapper => equals(o.get)
      case o: LookupElement => o.getLookupString.equals(get.getLookupString)
      case _ => false
    }
  }

  object LookupElementWrapper {
    def fromString(s: String): LookupElementWrapper = {
      new LookupElementWrapper(LookupElementUtil.fromString(s))
    }
  }

  private class Handler(project: Project) extends FileDocumentManagerListener {
    override def fileContentLoaded(file: VirtualFile, document: Document): Unit = {
      val app = ApplicationManager.getApplication
      app.runReadAction({ () =>
        Option(PsiManager.getInstance(project).findFile(file)).foreach {
          case psiFile: HaskellFile =>
            app.invokeLater({ () =>
              HaskellCompletionCacheLoader.getService(project).updateCache(psiFile, force = false)
            }: Runnable)
          case _ => // noop
        }
      }: Runnable)
    }
  }
}
