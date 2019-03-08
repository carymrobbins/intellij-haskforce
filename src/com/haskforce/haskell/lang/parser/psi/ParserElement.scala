package com.haskforce.haskell.lang.parser.psi

import com.haskforce.HaskellLanguage
import com.intellij.psi.tree.IElementType

sealed abstract class ParserElement(name: String)
  extends IElementType(name, HaskellLanguage.INSTANCE)

object Elements {

  case object HASKELL_FILE extends ParserElement("HASKELL_FILE")
  case object UNKNOWN extends ParserElement("UNKNOWN")
  case object MODULE_NAME extends ParserElement("MODULE_NAME")
  case object MODULE_DECL extends ParserElement("MODULE_DECL")
  case object IMPORT_MODULE extends ParserElement("IMPORT_MODULE")
}
