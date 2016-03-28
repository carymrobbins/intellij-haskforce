package com.haskforce.highlighting.annotation.external

import com.haskforce.highlighting.annotation.Problems

/** A component which provides problems to be highlighted in the editor. */
trait ProblemsProvider {
  def getProblems: Option[Problems]
}
