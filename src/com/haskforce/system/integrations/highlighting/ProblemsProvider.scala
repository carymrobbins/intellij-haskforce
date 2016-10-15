package com.haskforce.system.integrations.highlighting

/** A component which provides problems to be highlighted for an PsiFile in the editor. */
trait ProblemsProvider {
  /**
    * returns the Problems for the PsiFile
    */
  def getProblems: Option[java.util.List[HaskellProblem]]
}
