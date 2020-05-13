package com.haskforce.codeInsight

import java.util

import com.haskforce.codeInsight.HaskellCompletionCacheLoader.LookupElementWrapper
import com.haskforce.psi.{HaskellFile, HaskellPsiUtil}
import com.intellij.AppTopics
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.project.{Project, ProjectManagerListener}
import com.intellij.openapi.util.Computable
import com.intellij.openapi.vfs._
import com.intellij.psi.{PsiFile, PsiManager}

import scala.annotation.tailrec

/** Loads the completion cache for any Haskell files we have open. */
class HaskellCompletionCacheLoader extends ProjectManagerListener {

  override def projectOpened(project: Project): Unit = {
    project.getMessageBus.connect().subscribe(AppTopics.FILE_DOCUMENT_SYNC, new MyHandler(project))
  }

  val cache = new HaskellCompletionCacheLoader.Cache()


  class MyHandler(project : Project) extends FileDocumentManagerListener {
    override def fileContentLoaded(file: VirtualFile, document: Document): Unit = {
      val app = ApplicationManager.getApplication
      app.runReadAction({ () =>
        Option(PsiManager.getInstance(project).findFile(file)).foreach {
          case psiFile: HaskellFile =>
            app.invokeLater({ () =>
              updateCache(psiFile, force = false)
            }: Runnable)
          case _ => // noop
        }
      }: Runnable)
    }
  }

  def forceUpdateCache(file: PsiFile): Unit = updateCache(file, force = true)

  def updateCache(file: PsiFile, force: Boolean): Unit = {
    ApplicationManager.getApplication.executeOnPooledThread({ () =>
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
    }: Runnable)
    ()
  }

  private def putWrappers(s: util.Set[LookupElementWrapper], xs: Array[String]) = {
    util.Collections.addAll[LookupElementWrapper](s, xs.map(LookupElementWrapper.fromString): _*)
  }

  private def putStrings(s: util.Set[String], xs: Array[String]) = {
    util.Collections.addAll[String](s, xs: _*)
  }

  private def updateModuleSymbols(file: PsiFile): Unit = {
    ModuleSymbolsProviderFactory.get(file).foreach { provider =>
      val imports = ApplicationManager.getApplication.runReadAction({ () =>
        HaskellPsiUtil.parseImports(file)
      }: Computable[util.List[HaskellPsiUtil.Import]])
      imports.forEach { imp =>
        val syms = provider.getSymbols(imp.module)
        val symSet = new util.HashSet[LookupElementWrapper](
          util.Arrays.asList(syms.map(
            b => new LookupElementWrapper(b.toLookupElement)
          ): _*)
        )
        Option(cache.moduleSymbols.get(imp.module)) match {
          case None => cache.moduleSymbols.put(imp.module, symSet)
          case Some(currentSymList) => currentSymList.addAll(symSet)
        }
        ()
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
