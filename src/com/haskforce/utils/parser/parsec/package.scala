package com.haskforce.utils.parser

import com.intellij.lang.PsiBuilder
import scalaz.Reader

package object parsec {
  type Psi[A] = Reader[PsiBuilder, A]
  def Psi[A](f: PsiBuilder => A): Psi[A] = Reader(f)
}
