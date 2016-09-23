package com.haskforce.system.integrations.highlighting

/** A component which provides problems to be highlighted in the editor. */
trait ProblemsProvider {
  def getProblems: Option[Problems]
}
