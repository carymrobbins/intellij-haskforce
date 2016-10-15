package com.haskforce.system.integrations.highlighting

import com.intellij.psi.PsiFile

/** Generalized factory for getting a configured ProblemsProvider in HaskellExternalAnnotator. */
trait ProblemsProviderFactory {
  /**
    * returns the (optional) ProblemsProvider for the PsiFile
    */
  def get(psiFile: PsiFile): Option[ProblemsProvider]
}
