package com.haskforce.cabal.psi

import com.intellij.psi.tree.IElementType

import com.haskforce.cabal.CabalLanguage

class CabalHighlighterTokenType(val debugName: String)
  extends IElementType(debugName, CabalLanguage.INSTANCE)

class CabalTokenType(val debugName: String, val yyval: Short)
  extends IElementType(debugName, CabalLanguage.INSTANCE)

class CabalSymbolTokenType(debugName: String, yyval: Short) extends CabalTokenType(debugName, yyval)
class CabalOperatorTokenType(debugName: String, yyval: Short) extends CabalTokenType(debugName, yyval)
class CabalComparatorTokenType(debugName: String, yyval: Short) extends CabalTokenType(debugName, yyval)
class CabalLogicalTokenType(debugName: String, yyval: Short) extends CabalTokenType(debugName, yyval)

class CabalWordLikeTokenType(debugName: String, yyval: Short) extends CabalTokenType(debugName, yyval)
class CabalIdentTokenType(debugName: String, yyval: Short) extends CabalWordLikeTokenType(debugName, yyval)
class CabalNumericTokenType(debugName: String, yyval: Short) extends CabalWordLikeTokenType(debugName, yyval)
class CabalFieldKeyTokenType(debugName: String, yyval: Short) extends CabalIdentTokenType(debugName, yyval)
class CabalStanzaKeyTokenType(debugName: String, yyval: Short) extends CabalIdentTokenType(debugName, yyval)

trait CabalFuncLikeTokenType

class CabalFuncNameTokenType(debugName: String, yyval: Short)
  extends CabalIdentTokenType(debugName, yyval)
  with CabalFuncLikeTokenType

class CabalFlagKeywordTokenType(debugName: String, yyval: Short)
  extends CabalStanzaKeyTokenType(debugName, yyval)
  with CabalFuncLikeTokenType

class CabalLayoutTokenType(debugName: String, yyval: Short) extends CabalTokenType(debugName, yyval)
