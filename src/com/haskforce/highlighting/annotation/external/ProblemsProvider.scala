package com.haskforce.highlighting.annotation.external

import com.haskforce.highlighting.annotation.Problems

/**
 * Created by crobbins on 2/9/16.
 */
trait ProblemsProvider {
  def getProblems: WrappedFuture[Option[Problems]]
}
