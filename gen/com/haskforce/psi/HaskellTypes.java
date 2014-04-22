// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.psi.impl.*;

public interface HaskellTypes {

  IElementType ANYSEQ = new HaskellElementType("ANYSEQ");
  IElementType CONSYM = new HaskellElementType("CONSYM");
  IElementType MODULE_PREFIX = new HaskellElementType("MODULE_PREFIX");
  IElementType NCOMMENT = new HaskellElementType("NCOMMENT");
  IElementType PRAGMA = new HaskellElementType("PRAGMA");
  IElementType QCONID = new HaskellElementType("QCONID");
  IElementType QCONSYM = new HaskellElementType("QCONSYM");
  IElementType QINFIXVARID = new HaskellElementType("QINFIXVARID");
  IElementType QVARID = new HaskellElementType("QVARID");
  IElementType QVARSYM = new HaskellElementType("QVARSYM");
  IElementType RESERVEDID = new HaskellElementType("RESERVEDID");
  IElementType RESERVEDOP = new HaskellElementType("RESERVEDOP");
  IElementType RESERVEDOP_WITHOUT_CONS = new HaskellElementType("RESERVEDOP_WITHOUT_CONS");
  IElementType RESERVED_DECL = new HaskellElementType("RESERVED_DECL");
  IElementType RESERVED_EXPR = new HaskellElementType("RESERVED_EXPR");
  IElementType RESERVED_META = new HaskellElementType("RESERVED_META");
  IElementType RESERVED_VAR = new HaskellElementType("RESERVED_VAR");
  IElementType SPECIAL = new HaskellElementType("SPECIAL");
  IElementType SYMBOL = new HaskellElementType("SYMBOL");
  IElementType VARID = new HaskellElementType("VARID");
  IElementType VARSYM = new HaskellElementType("VARSYM");
  IElementType WHITECHAR = new HaskellElementType("WHITECHAR");

  IElementType AMPERSAND = new HaskellTokenType("&");
  IElementType AMPERSAT = new HaskellTokenType("@");
  IElementType ASTERISK = new HaskellTokenType("*");
  IElementType BACKSLASH = new HaskellTokenType("\\");
  IElementType BACKTICK = new HaskellTokenType("`");
  IElementType CARET = new HaskellTokenType("^");
  IElementType CHARESC = new HaskellTokenType("charesc");
  IElementType CHARTOKEN = new HaskellTokenType("chartoken");
  IElementType CLASSTOKEN = new HaskellTokenType("class");
  IElementType CLOSECOM = new HaskellTokenType("-}");
  IElementType CLOSEPRAGMA = new HaskellTokenType("#-}");
  IElementType COLON = new HaskellTokenType(":");
  IElementType COMMA = new HaskellTokenType(",");
  IElementType COMMENT = new HaskellTokenType("comment");
  IElementType CONID = new HaskellTokenType("conid");
  IElementType DOLLAR = new HaskellTokenType("$");
  IElementType DOUBLEARROW = new HaskellTokenType("=>");
  IElementType DOUBLECOLON = new HaskellTokenType("::");
  IElementType DOUBLEPERIOD = new HaskellTokenType("..");
  IElementType DOUBLEQUOTE = new HaskellTokenType("doublequote");
  IElementType EOL = new HaskellTokenType("EOL");
  IElementType EQUALS = new HaskellTokenType("=");
  IElementType EXLAMATION = new HaskellTokenType("!");
  IElementType FLOATTOKEN = new HaskellTokenType("floattoken");
  IElementType GREATERTHAN = new HaskellTokenType(">");
  IElementType HADDOCK = new HaskellTokenType("haddock");
  IElementType HASH = new HaskellTokenType("#");
  IElementType INTEGERTOKEN = new HaskellTokenType("integertoken");
  IElementType LBRACE = new HaskellTokenType("{");
  IElementType LBRACKET = new HaskellTokenType("[");
  IElementType LEFTARROW = new HaskellTokenType("<-");
  IElementType LESSTHAN = new HaskellTokenType("<");
  IElementType LINE_WS = new HaskellTokenType("LINE_WS");
  IElementType LPAREN = new HaskellTokenType("(");
  IElementType MINUS = new HaskellTokenType("-");
  IElementType NULLCHARACTER = new HaskellTokenType("\\&");
  IElementType OPENCOM = new HaskellTokenType("{-");
  IElementType OPENPRAGMA = new HaskellTokenType("{-#");
  IElementType PERCENT = new HaskellTokenType("%");
  IElementType PERIOD = new HaskellTokenType(".");
  IElementType PIPE = new HaskellTokenType("|");
  IElementType PLUS = new HaskellTokenType("+");
  IElementType QUESTION = new HaskellTokenType("?");
  IElementType RBRACE = new HaskellTokenType("}");
  IElementType RBRACKET = new HaskellTokenType("]");
  IElementType RIGHTARROW = new HaskellTokenType("->");
  IElementType RPAREN = new HaskellTokenType(")");
  IElementType SEMICOLON = new HaskellTokenType(";");
  IElementType SINGLEQUOTE = new HaskellTokenType("'");
  IElementType SLASH = new HaskellTokenType("/");
  IElementType STRINGTOKEN = new HaskellTokenType("stringtoken");
  IElementType TILDE = new HaskellTokenType("~");
  IElementType VARIDREGEXP = new HaskellTokenType("varidRegexp");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == ANYSEQ) {
        return new HaskellAnyseqImpl(node);
      }
      else if (type == CONSYM) {
        return new HaskellConsymImpl(node);
      }
      else if (type == MODULE_PREFIX) {
        return new HaskellModulePrefixImpl(node);
      }
      else if (type == NCOMMENT) {
        return new HaskellNcommentImpl(node);
      }
      else if (type == PRAGMA) {
        return new HaskellPragmaImpl(node);
      }
      else if (type == QCONID) {
        return new HaskellQconidImpl(node);
      }
      else if (type == QCONSYM) {
        return new HaskellQconsymImpl(node);
      }
      else if (type == QINFIXVARID) {
        return new HaskellQinfixvaridImpl(node);
      }
      else if (type == QVARID) {
        return new HaskellQvaridImpl(node);
      }
      else if (type == QVARSYM) {
        return new HaskellQvarsymImpl(node);
      }
      else if (type == RESERVEDID) {
        return new HaskellReservedidImpl(node);
      }
      else if (type == RESERVEDOP) {
        return new HaskellReservedopImpl(node);
      }
      else if (type == RESERVEDOP_WITHOUT_CONS) {
        return new HaskellReservedopWithoutConsImpl(node);
      }
      else if (type == RESERVED_DECL) {
        return new HaskellReservedDeclImpl(node);
      }
      else if (type == RESERVED_EXPR) {
        return new HaskellReservedExprImpl(node);
      }
      else if (type == RESERVED_META) {
        return new HaskellReservedMetaImpl(node);
      }
      else if (type == RESERVED_VAR) {
        return new HaskellReservedVarImpl(node);
      }
      else if (type == SPECIAL) {
        return new HaskellSpecialImpl(node);
      }
      else if (type == SYMBOL) {
        return new HaskellSymbolImpl(node);
      }
      else if (type == VARID) {
        return new HaskellVaridImpl(node);
      }
      else if (type == VARSYM) {
        return new HaskellVarsymImpl(node);
      }
      else if (type == WHITECHAR) {
        return new HaskellWhitecharImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
