package com.haskforce.haskell.codeInsight

import java.util

import scala.annotation.tailrec

import com.intellij.AppTopics
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs._
import com.intellij.psi.{PsiFile, PsiManager}

import com.haskforce.Implicits._
import com.haskforce.haskell.codeInsight.HaskellCompletionCacheLoader.LookupElementWrapper
import com.haskforce.haskell.psi.{HaskellFile, HaskellPsiUtil}
import com.haskforce.system.utils.SAMUtils

/** Loads the completion cache for any Haskell files we have open. */
class HaskellCompletionCacheLoader(project: Project) extends ProjectComponent {

  val cache = new HaskellCompletionCacheLoader.Cache()

  private val connection = project.getMessageBus.connect()

  override def initComponent(): Unit = {
    connection.subscribe(AppTopics.FILE_DOCUMENT_SYNC, new MyHandler)
  }

  override def disposeComponent(): Unit = {
    connection.disconnect()
  }

  override def projectOpened(): Unit = {}

  override def projectClosed(): Unit = {}

  override def getComponentName: String = getClass.getSimpleName

  class MyHandler extends FileDocumentManagerAdapter {
    override def fileContentLoaded(file: VirtualFile, document: Document): Unit = {
      val app = ApplicationManager.getApplication
      app.runReadAction(SAMUtils.runnable {
        Option(PsiManager.getInstance(project).findFile(file)).collect {
          case psiFile: HaskellFile =>
            app.invokeLater(SAMUtils.runnable {
              updateCache(psiFile, force = false)
            })
        }
      })
    }
  }

  def forceUpdateCache(file: PsiFile): Unit = updateCache(file, force = true)

  def updateCache(file: PsiFile, force: Boolean): Unit = {
    ApplicationManager.getApplication.executeOnPooledThread(SAMUtils.runnable {
      if (force || cache.ghcFlags.isEmpty) {
        CompilerFlagsProviderFactory.get(file).foreach { provider =>
          putStrings(cache.ghcFlags, provider.getFlags)
        }
      }
      if (force || cache.visibleModules.isEmpty) {
        VisibleModulesProviderFactory.get(file).foreach { provider =>
          putStrings(cache.visibleModules, provider.getVisibleModules)
        }
      }
      if (force || cache.languageExtensions.isEmpty) {
        LanguageExtensionsProviderFactory.get(file).foreach { provider =>
          putWrappers(cache.languageExtensions, provider.getLanguages)
        }
      }
      if (force || cache.moduleSymbols.isEmpty) {
        updateModuleSymbols(file)
      }
    })
  }

  private def putWrappers(s: util.Set[LookupElementWrapper], xs: Array[String]) = {
    util.Collections.addAll[LookupElementWrapper](s, xs.map(LookupElementWrapper.fromString): _*)
  }

  private def putStrings(s: util.Set[String], xs: Array[String]) = {
    util.Collections.addAll[String](s, xs: _*)
  }

  private def updateModuleSymbols(file: PsiFile): Unit = {
    ModuleSymbolsProviderFactory.get(file).foreach { provider =>
      val imports = ApplicationManager.getApplication.runReadAction(SAMUtils.computable(
        HaskellPsiUtil.parseImports(file)
      ))
      imports.foreach { imp =>
        val syms = provider.getSymbols(imp.module)
        val symSet = new util.HashSet(util.Arrays.asList(syms.map(
          b => new LookupElementWrapper(b.toLookupElement)
        ): _*))
        Option(cache.moduleSymbols.get(imp.module)) match {
          case None => cache.moduleSymbols.put(imp.module, symSet)
          case Some(currentSymList) => currentSymList.addAll(symSet)
        }
      }
    }
  }
}

object HaskellCompletionCacheLoader {
  def get(project: Project): HaskellCompletionCacheLoader = {
    project.getComponent(classOf[HaskellCompletionCacheLoader])
  }

  final class Cache {
    val ghcFlags: util.Set[String] = new util.HashSet(300)
    val visibleModules: util.Set[String] = new util.HashSet(1000)
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
}
