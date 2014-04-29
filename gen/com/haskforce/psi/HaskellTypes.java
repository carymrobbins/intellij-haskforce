// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.psi.impl.*;

public interface HaskellTypes {

  IElementType AEXP = new HaskellElementType("AEXP");
  IElementType ALT = new HaskellElementType("ALT");
  IElementType ALTS = new HaskellElementType("ALTS");
  IElementType ANYSEQ = new HaskellElementType("ANYSEQ");
  IElementType APAT = new HaskellElementType("APAT");
  IElementType ATYPE = new HaskellElementType("ATYPE");
  IElementType BODY = new HaskellElementType("BODY");
  IElementType CDECL = new HaskellElementType("CDECL");
  IElementType CLASSS = new HaskellElementType("CLASSS");
  IElementType CNAME = new HaskellElementType("CNAME");
  IElementType CNAMES = new HaskellElementType("CNAMES");
  IElementType CON = new HaskellElementType("CON");
  IElementType CONOP = new HaskellElementType("CONOP");
  IElementType CONSTR = new HaskellElementType("CONSTR");
  IElementType CONSTRS = new HaskellElementType("CONSTRS");
  IElementType CONSYM = new HaskellElementType("CONSYM");
  IElementType CONTEXT = new HaskellElementType("CONTEXT");
  IElementType CPP = new HaskellElementType("CPP");
  IElementType EXP = new HaskellElementType("EXP");
  IElementType EXPORT = new HaskellElementType("EXPORT");
  IElementType EXPORTS = new HaskellElementType("EXPORTS");
  IElementType FATYPE = new HaskellElementType("FATYPE");
  IElementType FDECL = new HaskellElementType("FDECL");
  IElementType FEXP = new HaskellElementType("FEXP");
  IElementType FIXITY = new HaskellElementType("FIXITY");
  IElementType FRTYPE = new HaskellElementType("FRTYPE");
  IElementType FTYPE = new HaskellElementType("FTYPE");
  IElementType FUNLHS = new HaskellElementType("FUNLHS");
  IElementType GCON = new HaskellElementType("GCON");
  IElementType GCONSYM = new HaskellElementType("GCONSYM");
  IElementType GENDECL = new HaskellElementType("GENDECL");
  IElementType GUARD = new HaskellElementType("GUARD");
  IElementType IDECL = new HaskellElementType("IDECL");
  IElementType IMPDECL = new HaskellElementType("IMPDECL");
  IElementType IMPORTT = new HaskellElementType("IMPORTT");
  IElementType INFIXEXP = new HaskellElementType("INFIXEXP");
  IElementType INST = new HaskellElementType("INST");
  IElementType LEXP = new HaskellElementType("LEXP");
  IElementType LPAT = new HaskellElementType("LPAT");
  IElementType MODULE_PREFIX = new HaskellElementType("MODULE_PREFIX");
  IElementType NCOMMENT = new HaskellElementType("NCOMMENT");
  IElementType NEWCONSTR = new HaskellElementType("NEWCONSTR");
  IElementType OP = new HaskellElementType("OP");
  IElementType OPS = new HaskellElementType("OPS");
  IElementType PAT = new HaskellElementType("PAT");
  IElementType PRAGMA = new HaskellElementType("PRAGMA");
  IElementType PSTRINGTOKEN = new HaskellElementType("PSTRINGTOKEN");
  IElementType QCON = new HaskellElementType("QCON");
  IElementType QCONID = new HaskellElementType("QCONID");
  IElementType QCONOP = new HaskellElementType("QCONOP");
  IElementType QCONSYM = new HaskellElementType("QCONSYM");
  IElementType QINFIXCONID = new HaskellElementType("QINFIXCONID");
  IElementType QINFIXVARID = new HaskellElementType("QINFIXVARID");
  IElementType QOP = new HaskellElementType("QOP");
  IElementType QTYCLS = new HaskellElementType("QTYCLS");
  IElementType QTYCON = new HaskellElementType("QTYCON");
  IElementType QUAL = new HaskellElementType("QUAL");
  IElementType QVAR = new HaskellElementType("QVAR");
  IElementType QVARID = new HaskellElementType("QVARID");
  IElementType QVAROP = new HaskellElementType("QVAROP");
  IElementType QVARS = new HaskellElementType("QVARS");
  IElementType QVARSYM = new HaskellElementType("QVARSYM");
  IElementType RESERVEDOP = new HaskellElementType("RESERVEDOP");
  IElementType RHS = new HaskellElementType("RHS");
  IElementType SCONTEXT = new HaskellElementType("SCONTEXT");
  IElementType SPECIAL = new HaskellElementType("SPECIAL");
  IElementType STMT = new HaskellElementType("STMT");
  IElementType STMTS = new HaskellElementType("STMTS");
  IElementType SYMBOL = new HaskellElementType("SYMBOL");
  IElementType TYCLS = new HaskellElementType("TYCLS");
  IElementType TYCON = new HaskellElementType("TYCON");
  IElementType TYPEE = new HaskellElementType("TYPEE");
  IElementType TYVAR = new HaskellElementType("TYVAR");
  IElementType VAROP = new HaskellElementType("VAROP");
  IElementType VARS = new HaskellElementType("VARS");
  IElementType VARSYM = new HaskellElementType("VARSYM");
  IElementType WHITECHAR = new HaskellElementType("WHITECHAR");

  IElementType AMPERSAND = new HaskellTokenType("&");
  IElementType AMPERSAT = new HaskellTokenType("@");
  IElementType AS = new HaskellTokenType("as");
  IElementType ASTERISK = new HaskellTokenType("*");
  IElementType BACKSLASH = new HaskellTokenType("\\");
  IElementType BACKTICK = new HaskellTokenType("`");
  IElementType CARET = new HaskellTokenType("^");
  IElementType CASE = new HaskellTokenType("case");
  IElementType CHARTOKEN = new HaskellTokenType("chartoken");
  IElementType CLASSTOKEN = new HaskellTokenType("class");
  IElementType CLOSECOM = new HaskellTokenType("-}");
  IElementType CLOSEPRAGMA = new HaskellTokenType("#-}");
  IElementType COLON = new HaskellTokenType(":");
  IElementType COMMA = new HaskellTokenType(",");
  IElementType COMMENT = new HaskellTokenType("comment");
  IElementType CONID = new HaskellTokenType("conid");
  IElementType CPPELSE = new HaskellTokenType("#else");
  IElementType CPPENDIF = new HaskellTokenType("#endif");
  IElementType CPPIF = new HaskellTokenType("CPPIF");
  IElementType DASHES = new HaskellTokenType("dashes");
  IElementType DATA = new HaskellTokenType("data");
  IElementType DEFAULT = new HaskellTokenType("default");
  IElementType DERIVING = new HaskellTokenType("deriving");
  IElementType DO = new HaskellTokenType("do");
  IElementType DOLLAR = new HaskellTokenType("$");
  IElementType DOUBLEARROW = new HaskellTokenType("=>");
  IElementType DOUBLECOLON = new HaskellTokenType("::");
  IElementType DOUBLEPERIOD = new HaskellTokenType("..");
  IElementType DOUBLEQUOTE = new HaskellTokenType("doublequote");
  IElementType ELSE = new HaskellTokenType("else");
  IElementType EOL = new HaskellTokenType("EOL");
  IElementType EQUALS = new HaskellTokenType("=");
  IElementType EXLAMATION = new HaskellTokenType("!");
  IElementType FLOATTOKEN = new HaskellTokenType("floattoken");
  IElementType FOREIGN = new HaskellTokenType("foreign");
  IElementType GREATERTHAN = new HaskellTokenType(">");
  IElementType HADDOCK = new HaskellTokenType("haddock");
  IElementType HASH = new HaskellTokenType("#");
  IElementType HIDING = new HaskellTokenType("hiding");
  IElementType IF = new HaskellTokenType("if");
  IElementType IMPORT = new HaskellTokenType("import");
  IElementType IN = new HaskellTokenType("in");
  IElementType INFIX = new HaskellTokenType("infix");
  IElementType INFIXL = new HaskellTokenType("infixl");
  IElementType INFIXR = new HaskellTokenType("infixr");
  IElementType INSTANCE = new HaskellTokenType("instance");
  IElementType INTEGERTOKEN = new HaskellTokenType("integertoken");
  IElementType LBRACE = new HaskellTokenType("{");
  IElementType LBRACKET = new HaskellTokenType("[");
  IElementType LEFTARROW = new HaskellTokenType("<-");
  IElementType LESSTHAN = new HaskellTokenType("<");
  IElementType LET = new HaskellTokenType("let");
  IElementType LINE_WS = new HaskellTokenType("LINE_WS");
  IElementType LPAREN = new HaskellTokenType("(");
  IElementType MINUS = new HaskellTokenType("-");
  IElementType MODULE = new HaskellTokenType("module");
  IElementType NEWTYPE = new HaskellTokenType("newtype");
  IElementType NULLCHARACTER = new HaskellTokenType("\\&");
  IElementType OF = new HaskellTokenType("of");
  IElementType OPENCOM = new HaskellTokenType("{-");
  IElementType OPENPRAGMA = new HaskellTokenType("{-#");
  IElementType PERCENT = new HaskellTokenType("%");
  IElementType PERIOD = new HaskellTokenType(".");
  IElementType PIPE = new HaskellTokenType("|");
  IElementType PLUS = new HaskellTokenType("+");
  IElementType QUALIFIED = new HaskellTokenType("qualified");
  IElementType QUESTION = new HaskellTokenType("?");
  IElementType RBRACE = new HaskellTokenType("}");
  IElementType RBRACKET = new HaskellTokenType("]");
  IElementType RIGHTARROW = new HaskellTokenType("->");
  IElementType RPAREN = new HaskellTokenType(")");
  IElementType SEMICOLON = new HaskellTokenType(";");
  IElementType SINGLEQUOTE = new HaskellTokenType("'");
  IElementType SLASH = new HaskellTokenType("/");
  IElementType STRINGTOKEN = new HaskellTokenType("stringtoken");
  IElementType THEN = new HaskellTokenType("then");
  IElementType THQUOTE = new HaskellTokenType("''");
  IElementType TILDE = new HaskellTokenType("~");
  IElementType TYPE = new HaskellTokenType("type");
  IElementType VARIDREGEXP = new HaskellTokenType("varidRegexp");
  IElementType WHERE = new HaskellTokenType("where");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == AEXP) {
        return new HaskellAexpImpl(node);
      }
      else if (type == ALT) {
        return new HaskellAltImpl(node);
      }
      else if (type == ALTS) {
        return new HaskellAltsImpl(node);
      }
      else if (type == ANYSEQ) {
        return new HaskellAnyseqImpl(node);
      }
      else if (type == APAT) {
        return new HaskellApatImpl(node);
      }
      else if (type == ATYPE) {
        return new HaskellAtypeImpl(node);
      }
      else if (type == BODY) {
        return new HaskellBodyImpl(node);
      }
      else if (type == CDECL) {
        return new HaskellCdeclImpl(node);
      }
      else if (type == CLASSS) {
        return new HaskellClasssImpl(node);
      }
      else if (type == CNAME) {
        return new HaskellCnameImpl(node);
      }
      else if (type == CNAMES) {
        return new HaskellCnamesImpl(node);
      }
      else if (type == CON) {
        return new HaskellConImpl(node);
      }
      else if (type == CONOP) {
        return new HaskellConopImpl(node);
      }
      else if (type == CONSTR) {
        return new HaskellConstrImpl(node);
      }
      else if (type == CONSTRS) {
        return new HaskellConstrsImpl(node);
      }
      else if (type == CONSYM) {
        return new HaskellConsymImpl(node);
      }
      else if (type == CONTEXT) {
        return new HaskellContextImpl(node);
      }
      else if (type == CPP) {
        return new HaskellCppImpl(node);
      }
      else if (type == EXP) {
        return new HaskellExpImpl(node);
      }
      else if (type == EXPORT) {
        return new HaskellExportImpl(node);
      }
      else if (type == EXPORTS) {
        return new HaskellExportsImpl(node);
      }
      else if (type == FATYPE) {
        return new HaskellFatypeImpl(node);
      }
      else if (type == FDECL) {
        return new HaskellFdeclImpl(node);
      }
      else if (type == FEXP) {
        return new HaskellFexpImpl(node);
      }
      else if (type == FIXITY) {
        return new HaskellFixityImpl(node);
      }
      else if (type == FRTYPE) {
        return new HaskellFrtypeImpl(node);
      }
      else if (type == FTYPE) {
        return new HaskellFtypeImpl(node);
      }
      else if (type == FUNLHS) {
        return new HaskellFunlhsImpl(node);
      }
      else if (type == GCON) {
        return new HaskellGconImpl(node);
      }
      else if (type == GCONSYM) {
        return new HaskellGconsymImpl(node);
      }
      else if (type == GENDECL) {
        return new HaskellGendeclImpl(node);
      }
      else if (type == GUARD) {
        return new HaskellGuardImpl(node);
      }
      else if (type == IDECL) {
        return new HaskellIdeclImpl(node);
      }
      else if (type == IMPDECL) {
        return new HaskellImpdeclImpl(node);
      }
      else if (type == IMPORTT) {
        return new HaskellImporttImpl(node);
      }
      else if (type == INFIXEXP) {
        return new HaskellInfixexpImpl(node);
      }
      else if (type == INST) {
        return new HaskellInstImpl(node);
      }
      else if (type == LEXP) {
        return new HaskellLexpImpl(node);
      }
      else if (type == LPAT) {
        return new HaskellLpatImpl(node);
      }
      else if (type == MODULE_PREFIX) {
        return new HaskellModulePrefixImpl(node);
      }
      else if (type == NCOMMENT) {
        return new HaskellNcommentImpl(node);
      }
      else if (type == NEWCONSTR) {
        return new HaskellNewconstrImpl(node);
      }
      else if (type == OP) {
        return new HaskellOpImpl(node);
      }
      else if (type == OPS) {
        return new HaskellOpsImpl(node);
      }
      else if (type == PAT) {
        return new HaskellPatImpl(node);
      }
      else if (type == PRAGMA) {
        return new HaskellPragmaImpl(node);
      }
      else if (type == PSTRINGTOKEN) {
        return new HaskellPstringtokenImpl(node);
      }
      else if (type == QCON) {
        return new HaskellQconImpl(node);
      }
      else if (type == QCONID) {
        return new HaskellQconidImpl(node);
      }
      else if (type == QCONOP) {
        return new HaskellQconopImpl(node);
      }
      else if (type == QCONSYM) {
        return new HaskellQconsymImpl(node);
      }
      else if (type == QINFIXCONID) {
        return new HaskellQinfixconidImpl(node);
      }
      else if (type == QINFIXVARID) {
        return new HaskellQinfixvaridImpl(node);
      }
      else if (type == QOP) {
        return new HaskellQopImpl(node);
      }
      else if (type == QTYCLS) {
        return new HaskellQtyclsImpl(node);
      }
      else if (type == QTYCON) {
        return new HaskellQtyconImpl(node);
      }
      else if (type == QUAL) {
        return new HaskellQualImpl(node);
      }
      else if (type == QVAR) {
        return new HaskellQvarImpl(node);
      }
      else if (type == QVARID) {
        return new HaskellQvaridImpl(node);
      }
      else if (type == QVAROP) {
        return new HaskellQvaropImpl(node);
      }
      else if (type == QVARS) {
        return new HaskellQvarsImpl(node);
      }
      else if (type == QVARSYM) {
        return new HaskellQvarsymImpl(node);
      }
      else if (type == RESERVEDOP) {
        return new HaskellReservedopImpl(node);
      }
      else if (type == RHS) {
        return new HaskellRhsImpl(node);
      }
      else if (type == SCONTEXT) {
        return new HaskellScontextImpl(node);
      }
      else if (type == SPECIAL) {
        return new HaskellSpecialImpl(node);
      }
      else if (type == STMT) {
        return new HaskellStmtImpl(node);
      }
      else if (type == STMTS) {
        return new HaskellStmtsImpl(node);
      }
      else if (type == SYMBOL) {
        return new HaskellSymbolImpl(node);
      }
      else if (type == TYCLS) {
        return new HaskellTyclsImpl(node);
      }
      else if (type == TYCON) {
        return new HaskellTyconImpl(node);
      }
      else if (type == TYPEE) {
        return new HaskellTypeeImpl(node);
      }
      else if (type == TYVAR) {
        return new HaskellTyvarImpl(node);
      }
      else if (type == VAROP) {
        return new HaskellVaropImpl(node);
      }
      else if (type == VARS) {
        return new HaskellVarsImpl(node);
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
