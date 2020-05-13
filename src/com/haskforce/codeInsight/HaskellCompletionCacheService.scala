package com.haskforce.codeInsight

import java.util

import com.haskforce.codeInsight.HaskellCompletionCacheLoader.LookupElementWrapper
import com.haskforce.psi.HaskellPsiUtil
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.Computable
import com.intellij.psi.PsiFile

/**
 * project-service that manages the cache
 */
class HaskellCompletionCacheService {

  val cache = new HaskellCompletionCacheLoader.Cache()

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
