package com.haskforce.highlighting.annotation.external

import com.intellij.psi.PsiFile

/**
 * Created by crobbins on 2/9/16.
 */
trait ProblemsProviderFactory {
  def get(psiFile: PsiFile): Option[ProblemsProvider]
}
