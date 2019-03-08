package com.haskforce.utils.parser.parsec

import com.intellij.psi.tree.IElementType

sealed trait MarkResult[+O <: IElementType]
object MarkResult {
  final case class Done[+O <: IElementType](elementType: O) extends MarkResult[O]
  final case class Collapse[+O <: IElementType](elementType: O) extends MarkResult[O]
  final case class Error(message: String) extends MarkResult[Nothing]
}


