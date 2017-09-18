package com.haskforce.codeInsight

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet}
import com.intellij.util.ProcessingContext

/** Force reloads the completion cache upon user request invocation of completion info. */
class HaskellCacheReloadCompletionProvider extends CompletionProvider[CompletionParameters] {

  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ): Unit = {
    // If the invocation count is > 0, then the user explicitly invoked completion,
    // usually via ctrl+space. Use this event to reload the cache for further completions.
    if (parameters.getInvocationCount > 0) {
      val cache = HaskellCompletionCacheLoader.get(parameters.getPosition.getProject)
      cache.forceUpdateCache(parameters.getPosition.getContainingFile)
    }
  }
}
