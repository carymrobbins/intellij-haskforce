// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.psi.impl.*;

public interface HaskellTypes {

  IElementType ANY = new HaskellElementType("ANY");
  IElementType ANY_1 = new HaskellElementType("ANY_1");
  IElementType ASC_SYMBOL = new HaskellElementType("ASC_SYMBOL");
  IElementType CHARESC = new HaskellElementType("CHARESC");
  IElementType CHARTOKEN = new HaskellElementType("CHARTOKEN");
  IElementType COMMENT = new HaskellElementType("COMMENT");
  IElementType CONID = new HaskellElementType("CONID");
  IElementType CONSYM = new HaskellElementType("CONSYM");
  IElementType DECIMAL = new HaskellElementType("DECIMAL");
  IElementType DIGIT = new HaskellElementType("DIGIT");
  IElementType ESCAPE = new HaskellElementType("ESCAPE");
  IElementType EXPONENT = new HaskellElementType("EXPONENT");
  IElementType FLOATTOKEN = new HaskellElementType("FLOATTOKEN");
  IElementType GAP = new HaskellElementType("GAP");
  IElementType GRAPHIC = new HaskellElementType("GRAPHIC");
  IElementType HEXADECIMAL = new HaskellElementType("HEXADECIMAL");
  IElementType IGNOREDCHAR = new HaskellElementType("IGNOREDCHAR");
  IElementType INTEGERTOKEN = new HaskellElementType("INTEGERTOKEN");
  IElementType LARGE = new HaskellElementType("LARGE");
  IElementType LEXEME = new HaskellElementType("LEXEME");
  IElementType LITERAL = new HaskellElementType("LITERAL");
  IElementType MODID = new HaskellElementType("MODID");
  IElementType NCOMMENT = new HaskellElementType("NCOMMENT");
  IElementType NEWLINE = new HaskellElementType("NEWLINE");
  IElementType OCTAL = new HaskellElementType("OCTAL");
  IElementType QCONID = new HaskellElementType("QCONID");
  IElementType QCONSYM = new HaskellElementType("QCONSYM");
  IElementType QVARID = new HaskellElementType("QVARID");
  IElementType QVARSYM = new HaskellElementType("QVARSYM");
  IElementType RESERVEDID = new HaskellElementType("RESERVEDID");
  IElementType RESERVEDOP = new HaskellElementType("RESERVEDOP");
  IElementType RESERVEDOP_WITHOUT_CONS = new HaskellElementType("RESERVEDOP_WITHOUT_CONS");
  IElementType RESERVEDOP_WITH_CONS = new HaskellElementType("RESERVEDOP_WITH_CONS");
  IElementType SEQ = new HaskellElementType("SEQ");
  IElementType SMALL = new HaskellElementType("SMALL");
  IElementType SPECIAL = new HaskellElementType("SPECIAL");
  IElementType STRINGTOKEN = new HaskellElementType("STRINGTOKEN");
  IElementType SYMBOL = new HaskellElementType("SYMBOL");
  IElementType VARID = new HaskellElementType("VARID");
  IElementType VARSYM = new HaskellElementType("VARSYM");
  IElementType WHITECHAR = new HaskellElementType("WHITECHAR");
  IElementType WHITESPACE = new HaskellElementType("WHITESPACE");

  IElementType ACHARLOWER = new HaskellTokenType("a");
  IElementType ACHARUPPER = new HaskellTokenType("A");
  IElementType AMPERSAND = new HaskellTokenType("&");
  IElementType AMPERSAT = new HaskellTokenType("@");
  IElementType ASCDIGIT = new HaskellTokenType("ascDigit");
  IElementType ASCII = new HaskellTokenType("ascii");
  IElementType ASCLARGE = new HaskellTokenType("ascLarge");
  IElementType ASCSMALL = new HaskellTokenType("ascSmall");
  IElementType ASTERISK = new HaskellTokenType("*");
  IElementType BACKSLASH = new HaskellTokenType("\\");
  IElementType BACKTICK = new HaskellTokenType("`");
  IElementType BCHARLOWER = new HaskellTokenType("b");
  IElementType BCHARUPPER = new HaskellTokenType("B");
  IElementType CARET = new HaskellTokenType("^");
  IElementType CASE = new HaskellTokenType("case");
  IElementType CCHARLOWER = new HaskellTokenType("c");
  IElementType CCHARUPPER = new HaskellTokenType("C");
  IElementType CLASSTOKEN = new HaskellTokenType("class");
  IElementType CLOSECOM = new HaskellTokenType("-}");
  IElementType COLON = new HaskellTokenType(":");
  IElementType COMMA = new HaskellTokenType(",");
  IElementType DASHES = new HaskellTokenType("--");
  IElementType DATA = new HaskellTokenType("data");
  IElementType DCHARLOWER = new HaskellTokenType("d");
  IElementType DCHARUPPER = new HaskellTokenType("D");
  IElementType DEFAULT = new HaskellTokenType("default");
  IElementType DERIVING = new HaskellTokenType("deriving");
  IElementType DO = new HaskellTokenType("do");
  IElementType DOLLAR = new HaskellTokenType("$");
  IElementType DOUBLEARROW = new HaskellTokenType("=>");
  IElementType DOUBLECOLON = new HaskellTokenType("::");
  IElementType DOUBLEPERIOD = new HaskellTokenType("..");
  IElementType DOUBLEQUOTE = new HaskellTokenType("\"");
  IElementType ECHARLOWER = new HaskellTokenType("e");
  IElementType ECHARUPPER = new HaskellTokenType("E");
  IElementType ELSE = new HaskellTokenType("else");
  IElementType EQUALS = new HaskellTokenType("=");
  IElementType EXLAMATION = new HaskellTokenType("!");
  IElementType FCHARLOWER = new HaskellTokenType("f");
  IElementType FCHARUPPER = new HaskellTokenType("F");
  IElementType FOREIGN = new HaskellTokenType("foreign");
  IElementType FORMFEED = new HaskellTokenType("\\f");
  IElementType GCHARLOWER = new HaskellTokenType("g");
  IElementType GCHARUPPER = new HaskellTokenType("G");
  IElementType GREATERTHAN = new HaskellTokenType(">");
  IElementType HASH = new HaskellTokenType("#");
  IElementType HCHARLOWER = new HaskellTokenType("h");
  IElementType HCHARUPPER = new HaskellTokenType("H");
  IElementType HEXADECIMALPREFIX = new HaskellTokenType("hexadecimalPrefix");
  IElementType HEXIT = new HaskellTokenType("hexit");
  IElementType ICHARLOWER = new HaskellTokenType("i");
  IElementType ICHARUPPER = new HaskellTokenType("I");
  IElementType IF = new HaskellTokenType("if");
  IElementType IMPORT = new HaskellTokenType("import");
  IElementType IN = new HaskellTokenType("in");
  IElementType INFIX = new HaskellTokenType("infix");
  IElementType INFIXL = new HaskellTokenType("infixl");
  IElementType INFIXR = new HaskellTokenType("infixr");
  IElementType INSTANCE = new HaskellTokenType("instance");
  IElementType JCHARLOWER = new HaskellTokenType("j");
  IElementType JCHARUPPER = new HaskellTokenType("J");
  IElementType KCHARLOWER = new HaskellTokenType("k");
  IElementType KCHARUPPER = new HaskellTokenType("K");
  IElementType LBRACE = new HaskellTokenType("{");
  IElementType LBRACKET = new HaskellTokenType("[");
  IElementType LCHARLOWER = new HaskellTokenType("l");
  IElementType LCHARUPPER = new HaskellTokenType("L");
  IElementType LEFTARROW = new HaskellTokenType("<-");
  IElementType LESSTHAN = new HaskellTokenType("<");
  IElementType LET = new HaskellTokenType("let");
  IElementType LINEFEED = new HaskellTokenType("\\r");
  IElementType LPAREN = new HaskellTokenType("(");
  IElementType MCHARLOWER = new HaskellTokenType("m");
  IElementType MCHARUPPER = new HaskellTokenType("M");
  IElementType MINUS = new HaskellTokenType("-");
  IElementType MODULE = new HaskellTokenType("module");
  IElementType NCHARLOWER = new HaskellTokenType("n");
  IElementType NCHARUPPER = new HaskellTokenType("N");
  IElementType NEWTYPE = new HaskellTokenType("newtype");
  IElementType NULLCHARACTER = new HaskellTokenType("\\&");
  IElementType OCHARLOWER = new HaskellTokenType("o");
  IElementType OCHARUPPER = new HaskellTokenType("O");
  IElementType OCTALPREFIX = new HaskellTokenType("octalPrefix");
  IElementType OCTIT = new HaskellTokenType("octit");
  IElementType OF = new HaskellTokenType("of");
  IElementType OPENCOM = new HaskellTokenType("{-");
  IElementType PCHARLOWER = new HaskellTokenType("p");
  IElementType PCHARUPPER = new HaskellTokenType("P");
  IElementType PERCENT = new HaskellTokenType("%");
  IElementType PERIOD = new HaskellTokenType(".");
  IElementType PIPE = new HaskellTokenType("|");
  IElementType PLUS = new HaskellTokenType("+");
  IElementType QCHARLOWER = new HaskellTokenType("q");
  IElementType QCHARUPPER = new HaskellTokenType("Q");
  IElementType QUESTION = new HaskellTokenType("?");
  IElementType RBRACE = new HaskellTokenType("}");
  IElementType RBRACKET = new HaskellTokenType("]");
  IElementType RCHARLOWER = new HaskellTokenType("r");
  IElementType RCHARUPPER = new HaskellTokenType("R");
  IElementType RETURN = new HaskellTokenType("\\n");
  IElementType RIGHTARROW = new HaskellTokenType("->");
  IElementType RPAREN = new HaskellTokenType(")");
  IElementType SCHARLOWER = new HaskellTokenType("s");
  IElementType SCHARUPPER = new HaskellTokenType("S");
  IElementType SEMICOLON = new HaskellTokenType(";");
  IElementType SINGLEQUOTE = new HaskellTokenType("'");
  IElementType SLASH = new HaskellTokenType("/");
  IElementType SPACE = new HaskellTokenType(" ");
  IElementType TAB = new HaskellTokenType("\\t");
  IElementType TCHARLOWER = new HaskellTokenType("t");
  IElementType TCHARUPPER = new HaskellTokenType("T");
  IElementType THEN = new HaskellTokenType("then");
  IElementType TILDE = new HaskellTokenType("~");
  IElementType TYPE = new HaskellTokenType("type");
  IElementType UCHARLOWER = new HaskellTokenType("u");
  IElementType UCHARUPPER = new HaskellTokenType("U");
  IElementType UNDERSCORE = new HaskellTokenType("_");
  IElementType VCHARLOWER = new HaskellTokenType("v");
  IElementType VCHARUPPER = new HaskellTokenType("V");
  IElementType VERTAB = new HaskellTokenType("\\v");
  IElementType WCHARLOWER = new HaskellTokenType("w");
  IElementType WCHARUPPER = new HaskellTokenType("W");
  IElementType WHERE = new HaskellTokenType("where");
  IElementType XCHARLOWER = new HaskellTokenType("x");
  IElementType XCHARUPPER = new HaskellTokenType("X");
  IElementType YCHARLOWER = new HaskellTokenType("y");
  IElementType YCHARUPPER = new HaskellTokenType("Y");
  IElementType ZCHARLOWER = new HaskellTokenType("z");
  IElementType ZCHARUPPER = new HaskellTokenType("Z");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == ANY) {
        return new HaskellAnyImpl(node);
      }
      else if (type == ANY_1) {
        return new HaskellAny1Impl(node);
      }
      else if (type == ASC_SYMBOL) {
        return new HaskellAscSymbolImpl(node);
      }
      else if (type == CHARESC) {
        return new HaskellCharescImpl(node);
      }
      else if (type == CHARTOKEN) {
        return new HaskellChartokenImpl(node);
      }
      else if (type == COMMENT) {
        return new HaskellCommentImpl(node);
      }
      else if (type == CONID) {
        return new HaskellConidImpl(node);
      }
      else if (type == CONSYM) {
        return new HaskellConsymImpl(node);
      }
      else if (type == DECIMAL) {
        return new HaskellDecimalImpl(node);
      }
      else if (type == DIGIT) {
        return new HaskellDigitImpl(node);
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
      else if (type == GRAPHIC) {
        return new HaskellGraphicImpl(node);
      }
      else if (type == HEXADECIMAL) {
        return new HaskellHexadecimalImpl(node);
      }
      else if (type == IGNOREDCHAR) {
        return new HaskellIgnoredcharImpl(node);
      }
      else if (type == INTEGERTOKEN) {
        return new HaskellIntegertokenImpl(node);
      }
      else if (type == LARGE) {
        return new HaskellLargeImpl(node);
      }
      else if (type == LEXEME) {
        return new HaskellLexemeImpl(node);
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
      else if (type == NEWLINE) {
        return new HaskellNewlineImpl(node);
      }
      else if (type == OCTAL) {
        return new HaskellOctalImpl(node);
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
      else if (type == RESERVEDOP_WITH_CONS) {
        return new HaskellReservedopWithConsImpl(node);
      }
      else if (type == SEQ) {
        return new HaskellSeqImpl(node);
      }
      else if (type == SMALL) {
        return new HaskellSmallImpl(node);
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
      else if (type == WHITESPACE) {
        return new HaskellWhitespaceImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
