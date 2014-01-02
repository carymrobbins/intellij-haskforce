// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.psi.impl.*;

public interface HaskellTypes {

  IElementType ANYSEQ = new HaskellElementType("ANYSEQ");
  IElementType CHARTOKEN = new HaskellElementType("CHARTOKEN");
  IElementType CONSYM = new HaskellElementType("CONSYM");
  IElementType DECIMAL = new HaskellElementType("DECIMAL");
  IElementType ESCAPE = new HaskellElementType("ESCAPE");
  IElementType EXPONENT = new HaskellElementType("EXPONENT");
  IElementType FLOATTOKEN = new HaskellElementType("FLOATTOKEN");
  IElementType GAP = new HaskellElementType("GAP");
  IElementType INTEGERTOKEN = new HaskellElementType("INTEGERTOKEN");
  IElementType LITERAL = new HaskellElementType("LITERAL");
  IElementType MODID = new HaskellElementType("MODID");
  IElementType NCOMMENT = new HaskellElementType("NCOMMENT");
  IElementType QCONID = new HaskellElementType("QCONID");
  IElementType QCONSYM = new HaskellElementType("QCONSYM");
  IElementType QVARID = new HaskellElementType("QVARID");
  IElementType QVARSYM = new HaskellElementType("QVARSYM");
  IElementType RESERVEDID = new HaskellElementType("RESERVEDID");
  IElementType RESERVEDOP = new HaskellElementType("RESERVEDOP");
  IElementType RESERVEDOP_WITHOUT_CONS = new HaskellElementType("RESERVEDOP_WITHOUT_CONS");
  IElementType SPECIAL = new HaskellElementType("SPECIAL");
  IElementType STRINGTOKEN = new HaskellElementType("STRINGTOKEN");
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
  IElementType CLASSTOKEN = new HaskellTokenType("class");
  IElementType CLOSECOM = new HaskellTokenType("-}");
  IElementType COLON = new HaskellTokenType(":");
  IElementType COMMA = new HaskellTokenType(",");
  IElementType COMMENT = new HaskellTokenType("comment");
  IElementType CONID = new HaskellTokenType("conid");
  IElementType DASHES = new HaskellTokenType("dashes");
  IElementType DIGIT = new HaskellTokenType("digit");
  IElementType DOLLAR = new HaskellTokenType("$");
  IElementType DOUBLEARROW = new HaskellTokenType("=>");
  IElementType DOUBLECOLON = new HaskellTokenType("::");
  IElementType DOUBLEPERIOD = new HaskellTokenType("..");
  IElementType DOUBLEQUOTE = new HaskellTokenType("doublequote");
  IElementType EQUALS = new HaskellTokenType("=");
  IElementType EXLAMATION = new HaskellTokenType("!");
  IElementType EXPONENTPREFIX = new HaskellTokenType("exponentPrefix");
  IElementType GRAPHIC = new HaskellTokenType("graphic");
  IElementType GREATERTHAN = new HaskellTokenType(">");
  IElementType HADDOCK = new HaskellTokenType("haddock");
  IElementType HASH = new HaskellTokenType("#");
  IElementType HEXADECIMALESCAPE = new HaskellTokenType("hexadecimalEscape");
  IElementType HEXADECIMALLITERAL = new HaskellTokenType("hexadecimalLiteral");
  IElementType LBRACE = new HaskellTokenType("{");
  IElementType LBRACKET = new HaskellTokenType("[");
  IElementType LEFTARROW = new HaskellTokenType("<-");
  IElementType LESSTHAN = new HaskellTokenType("<");
  IElementType LPAREN = new HaskellTokenType("(");
  IElementType MINUS = new HaskellTokenType("-");
  IElementType NULLCHARACTER = new HaskellTokenType("\\&");
  IElementType OCTALESCAPE = new HaskellTokenType("octalEscape");
  IElementType OCTALLITERAL = new HaskellTokenType("octalLiteral");
  IElementType OPENCOM = new HaskellTokenType("{-");
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
  IElementType SPACE = new HaskellTokenType(" ");
  IElementType TILDE = new HaskellTokenType("~");
  IElementType VARIDREGEXP = new HaskellTokenType("varidRegexp");
  IElementType WHITEESCAPES = new HaskellTokenType("whiteEscapes");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == ANYSEQ) {
        return new HaskellAnyseqImpl(node);
      }
      else if (type == CHARTOKEN) {
        return new HaskellChartokenImpl(node);
      }
      else if (type == CONSYM) {
        return new HaskellConsymImpl(node);
      }
      else if (type == DECIMAL) {
        return new HaskellDecimalImpl(node);
      }
      else if (type == ESCAPE) {
        return new HaskellEscapeImpl(node);
      }
      else if (type == EXPONENT) {
        return new HaskellExponentImpl(node);
      }
      else if (type == FLOATTOKEN) {
        return new HaskellFloattokenImpl(node);
      }
      else if (type == GAP) {
        return new HaskellGapImpl(node);
      }
      else if (type == INTEGERTOKEN) {
        return new HaskellIntegertokenImpl(node);
      }
      else if (type == LITERAL) {
        return new HaskellLiteralImpl(node);
      }
      else if (type == MODID) {
        return new HaskellModidImpl(node);
      }
      else if (type == NCOMMENT) {
        return new HaskellNcommentImpl(node);
      }
      else if (type == QCONID) {
        return new HaskellQconidImpl(node);
      }
      else if (type == QCONSYM) {
        return new HaskellQconsymImpl(node);
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
      else if (type == SPECIAL) {
        return new HaskellSpecialImpl(node);
      }
      else if (type == STRINGTOKEN) {
        return new HaskellStringtokenImpl(node);
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
