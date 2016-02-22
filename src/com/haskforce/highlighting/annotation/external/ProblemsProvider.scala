package com.haskforce.highlighting.annotation.external

import com.haskforce.highlighting.annotation.Problems
import com.haskforce.utils.WrappedFuture

/** A component which provides problems to be highlighted in the editor. */
trait ProblemsProvider {
  def getProblems: WrappedFuture[Option[Problems]]
}
