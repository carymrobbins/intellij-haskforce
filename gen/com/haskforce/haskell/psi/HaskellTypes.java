// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.haskell.psi.impl.HaskellElementTypeFactory;
import com.haskforce.haskell.psi.impl.*;

public interface HaskellTypes {

  IElementType AKIND = new HaskellElementType("AKIND");
  IElementType ALT = new HaskellElementType("ALT");
  IElementType ATYPE = new HaskellElementType("ATYPE");
  IElementType BKIND = new HaskellElementType("BKIND");
  IElementType BODY = new HaskellElementType("BODY");
  IElementType CDECL = new HaskellElementType("CDECL");
  IElementType CLASSDECL = new HaskellElementType("CLASSDECL");
  IElementType CLSCONTEXT = new HaskellElementType("CLSCONTEXT");
  IElementType CON = new HaskellElementType("CON");
  IElementType CONID = HaskellElementTypeFactory.factory("CONID");
  IElementType CONOP = new HaskellElementType("CONOP");
  IElementType CONSTR = new HaskellElementType("CONSTR");
  IElementType CONSYM = new HaskellElementType("CONSYM");
  IElementType CTYPE = new HaskellElementType("CTYPE");
  IElementType DATADECL = new HaskellElementType("DATADECL");
  IElementType DEFAULTDECL = new HaskellElementType("DEFAULTDECL");
  IElementType DERIVINGDECL = new HaskellElementType("DERIVINGDECL");
  IElementType EXP = new HaskellElementType("EXP");
  IElementType EXPORT = new HaskellElementType("EXPORT");
  IElementType EXPORTS = new HaskellElementType("EXPORTS");
  IElementType EXPORTSEMPTY = new HaskellElementType("EXPORTSEMPTY");
  IElementType FIXITY = new HaskellElementType("FIXITY");
  IElementType FOREIGNDECL = new HaskellElementType("FOREIGNDECL");
  IElementType FUNORPATDECL = new HaskellElementType("FUNORPATDECL");
  IElementType GCONSYM = new HaskellElementType("GCONSYM");
  IElementType GENDECL = new HaskellElementType("GENDECL");
  IElementType GUARD = new HaskellElementType("GUARD");
  IElementType IDECL = new HaskellElementType("IDECL");
  IElementType IMPDECL = new HaskellElementType("IMPDECL");
  IElementType IMPEMPTY = new HaskellElementType("IMPEMPTY");
  IElementType IMPORTT = new HaskellElementType("IMPORTT");
  IElementType INSTANCEDECL = new HaskellElementType("INSTANCEDECL");
  IElementType KIND = new HaskellElementType("KIND");
  IElementType LETEXP = new HaskellElementType("LETEXP");
  IElementType LISTLIKE = new HaskellElementType("LISTLIKE");
  IElementType MODULEDECL = new HaskellElementType("MODULEDECL");
  IElementType NEWCONSTR = new HaskellElementType("NEWCONSTR");
  IElementType NEWTYPEDECL = new HaskellElementType("NEWTYPEDECL");
  IElementType OP = new HaskellElementType("OP");
  IElementType OQTYCON = new HaskellElementType("OQTYCON");
  IElementType PAT = new HaskellElementType("PAT");
  IElementType PPRAGMA = new HaskellElementType("PPRAGMA");
  IElementType PSTRINGTOKEN = new HaskellElementType("PSTRINGTOKEN");
  IElementType QCON = new HaskellElementType("QCON");
  IElementType QCONID = new HaskellElementType("QCONID");
  IElementType QCONOP = new HaskellElementType("QCONOP");
  IElementType QCONSYM = new HaskellElementType("QCONSYM");
  IElementType QOP = new HaskellElementType("QOP");
  IElementType QQBLOB = new HaskellElementType("QQBLOB");
  IElementType QTYCLS = new HaskellElementType("QTYCLS");
  IElementType QTYCON = new HaskellElementType("QTYCON");
  IElementType QTYCONOP = new HaskellElementType("QTYCONOP");
  IElementType QTYCONSYM = new HaskellElementType("QTYCONSYM");
  IElementType QVAR = new HaskellElementType("QVAR");
  IElementType QVARID = new HaskellElementType("QVARID");
  IElementType QVAROP = new HaskellElementType("QVAROP");
  IElementType QVARS = new HaskellElementType("QVARS");
  IElementType QVARSYM = new HaskellElementType("QVARSYM");
  IElementType RHS = new HaskellElementType("RHS");
  IElementType SHEBANG = new HaskellElementType("SHEBANG");
  IElementType STMTS = new HaskellElementType("STMTS");
  IElementType TV_BNDR = new HaskellElementType("TV_BNDR");
  IElementType TYCLS = new HaskellElementType("TYCLS");
  IElementType TYCON = new HaskellElementType("TYCON");
  IElementType TYCONSYM = new HaskellElementType("TYCONSYM");
  IElementType TYPEDECL = new HaskellElementType("TYPEDECL");
  IElementType TYPEE = new HaskellElementType("TYPEE");
  IElementType TYVAR = new HaskellElementType("TYVAR");
  IElementType VARID = HaskellElementTypeFactory.factory("VARID");
  IElementType VAROP = new HaskellElementType("VAROP");
  IElementType VARS = new HaskellElementType("VARS");
  IElementType VARSYM = new HaskellElementType("VARSYM");

  IElementType AMPERSAND = new HaskellTokenType("&");
  IElementType AMPERSAT = new HaskellTokenType("@");
  IElementType AS = new HaskellTokenType("as");
  IElementType ASTERISK = new HaskellTokenType("*");
  IElementType BACKSLASH = new HaskellTokenType("\\");
  IElementType BACKTICK = new HaskellTokenType("`");
  IElementType BADSTRINGTOKEN = new HaskellTokenType("badstringtoken");
  IElementType CARET = new HaskellTokenType("^");
  IElementType CASE = new HaskellTokenType("case");
  IElementType CHARTOKEN = new HaskellTokenType("chartoken");
  IElementType CLASSTOKEN = new HaskellTokenType("class");
  IElementType CLOSECOM = new HaskellTokenType("-}");
  IElementType CLOSEPRAGMA = new HaskellTokenType("#-}");
  IElementType COLON = new HaskellTokenType(":");
  IElementType COMMA = new HaskellTokenType(",");
  IElementType COMMENT = new HaskellTokenType("comment");
  IElementType COMMENTTEXT = new HaskellTokenType("commenttext");
  IElementType CONIDREGEXP = new HaskellTokenType("conidRegexp");
  IElementType CONSYMTOK = new HaskellTokenType("Consym");
  IElementType CPPDEFINE = new HaskellTokenType("CPPDEFINE");
  IElementType CPPELIF = new HaskellTokenType("CPPELIF");
  IElementType CPPELSE = new HaskellTokenType("#else");
  IElementType CPPENDIF = new HaskellTokenType("#endif");
  IElementType CPPIF = new HaskellTokenType("CPPIF");
  IElementType CPPIFDEF = new HaskellTokenType("CPPIFDEF");
  IElementType CPPINCLUDE = new HaskellTokenType("CPPINCLUDE");
  IElementType CPPLINE = new HaskellTokenType("CPPLINE");
  IElementType CPPPRAGMA = new HaskellTokenType("CPPPRAGMA");
  IElementType CPPUNDEF = new HaskellTokenType("CPPUNDEF");
  IElementType DASHES = new HaskellTokenType("dashes");
  IElementType DATA = new HaskellTokenType("data");
  IElementType DEFAULT = new HaskellTokenType("default");
  IElementType DERIVING = new HaskellTokenType("deriving");
  IElementType DO = new HaskellTokenType("do");
  IElementType DOLLAR = new HaskellTokenType("$");
  IElementType DOUBLEARROW = new HaskellTokenType("=>");
  IElementType DOUBLECOLON = new HaskellTokenType("::");
  IElementType DOUBLEHASH = new HaskellTokenType("##");
  IElementType DOUBLEPERIOD = new HaskellTokenType("..");
  IElementType DOUBLEQUOTE = new HaskellTokenType("\"");
  IElementType ELSE = new HaskellTokenType("else");
  IElementType EQUALS = new HaskellTokenType("=");
  IElementType EXCLAMATION = new HaskellTokenType("!");
  IElementType EXPORTTOKEN = new HaskellTokenType("export");
  IElementType FALSE = new HaskellTokenType("false");
  IElementType FAMILYTOKEN = new HaskellTokenType("family");
  IElementType FLOATTOKEN = new HaskellTokenType("floattoken");
  IElementType FORALLTOKEN = new HaskellTokenType("forall");
  IElementType FOREIGN = new HaskellTokenType("foreign");
  IElementType GREATERTHAN = new HaskellTokenType(">");
  IElementType HADDOCK = new HaskellTokenType("haddock");
  IElementType HASH = new HaskellTokenType("#");
  IElementType HIDING = new HaskellTokenType("hiding");
  IElementType IDSPLICE = new HaskellTokenType("idsplice");
  IElementType IF = new HaskellTokenType("if");
  IElementType IMPORT = new HaskellTokenType("import");
  IElementType IN = new HaskellTokenType("in");
  IElementType INFIX = new HaskellTokenType("infix");
  IElementType INFIXL = new HaskellTokenType("infixl");
  IElementType INFIXR = new HaskellTokenType("infixr");
  IElementType INFIXVARID = new HaskellTokenType("Infix varid");
  IElementType INSTANCE = new HaskellTokenType("instance");
  IElementType INTEGERTOKEN = new HaskellTokenType("integertoken");
  IElementType LBRACE = new HaskellTokenType("{");
  IElementType LBRACKET = new HaskellTokenType("[");
  IElementType LCASETOK = new HaskellTokenType("\\case");
  IElementType LEFTARROW = new HaskellTokenType("<-");
  IElementType LESSTHAN = new HaskellTokenType("<");
  IElementType LET = new HaskellTokenType("let");
  IElementType LPAREN = new HaskellTokenType("(");
  IElementType LTHOPEN = new HaskellTokenType("[|");
  IElementType LUNBOXPAREN = new HaskellTokenType("(#");
  IElementType MDOTOK = new HaskellTokenType("mdo");
  IElementType MINUS = new HaskellTokenType("-");
  IElementType MODULETOKEN = new HaskellTokenType("module");
  IElementType NEWTYPE = new HaskellTokenType("newtype");
  IElementType NULLCHARACTER = new HaskellTokenType("\\&");
  IElementType OF = new HaskellTokenType("of");
  IElementType OPENCOM = new HaskellTokenType("{-");
  IElementType OPENPRAGMA = new HaskellTokenType("{-#");
  IElementType PARENSPLICE = new HaskellTokenType("$(");
  IElementType PERCENT = new HaskellTokenType("%");
  IElementType PERIOD = new HaskellTokenType(".");
  IElementType PIPE = new HaskellTokenType("|");
  IElementType PLUS = new HaskellTokenType("+");
  IElementType PRAGMA = new HaskellTokenType("PRAGMA");
  IElementType QQOPEN = new HaskellTokenType("Quasi-[");
  IElementType QQTEXT = new HaskellTokenType("Quasi-stuff");
  IElementType QUALIFIED = new HaskellTokenType("qualified");
  IElementType QUESTION = new HaskellTokenType("?");
  IElementType RBRACE = new HaskellTokenType("}");
  IElementType RBRACKET = new HaskellTokenType("]");
  IElementType RECTOK = new HaskellTokenType("rec");
  IElementType RIGHTARROW = new HaskellTokenType("->");
  IElementType RPAREN = new HaskellTokenType(")");
  IElementType RTHCLOSE = new HaskellTokenType("|]");
  IElementType RUNBOXPAREN = new HaskellTokenType("#)");
  IElementType SEMICOLON = new HaskellTokenType(";");
  IElementType SHEBANGPATH = new HaskellTokenType("Synthetic shebang path");
  IElementType SHEBANGSTART = new HaskellTokenType("Synthetic shebang start \"#!\"");
  IElementType SINGLEQUOTE = new HaskellTokenType("'");
  IElementType SLASH = new HaskellTokenType("/");
  IElementType STRINGTOKEN = new HaskellTokenType("STRINGTOKEN");
  IElementType THEN = new HaskellTokenType("then");
  IElementType THQUOTE = new HaskellTokenType("''");
  IElementType TILDE = new HaskellTokenType("~");
  IElementType TRUE = new HaskellTokenType("true");
  IElementType TYPE = new HaskellTokenType("type");
  IElementType UNDERSCORE = new HaskellTokenType("_");
  IElementType VARIDREGEXP = new HaskellTokenType("varidRegexp");
  IElementType VARSYMTOK = new HaskellTokenType("Varsym");
  IElementType VARSYMTOKPLUS = new HaskellTokenType("VarsymPlus");
  IElementType WHERE = new HaskellTokenType("where");
  IElementType WHITESPACELBRACETOK = new HaskellTokenType("Synthetic leftbrace");
  IElementType WHITESPACERBRACETOK = new HaskellTokenType("Synthetic rightbrace");
  IElementType WHITESPACESEMITOK = new HaskellTokenType("Synthetic semicolon");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == AKIND) {
        return new HaskellAkindImpl(node);
      }
      else if (type == ALT) {
        return new HaskellAltImpl(node);
      }
      else if (type == ATYPE) {
        return new HaskellAtypeImpl(node);
      }
      else if (type == BKIND) {
        return new HaskellBkindImpl(node);
      }
      else if (type == BODY) {
        return new HaskellBodyImpl(node);
      }
      else if (type == CDECL) {
        return new HaskellCdeclImpl(node);
      }
      else if (type == CLASSDECL) {
        return new HaskellClassdeclImpl(node);
      }
      else if (type == CLSCONTEXT) {
        return new HaskellClscontextImpl(node);
      }
      else if (type == CON) {
        return new HaskellConImpl(node);
      }
      else if (type == CONID) {
        return new HaskellConidImpl(node);
      }
      else if (type == CONOP) {
        return new HaskellConopImpl(node);
      }
      else if (type == CONSTR) {
        return new HaskellConstrImpl(node);
      }
      else if (type == CONSYM) {
        return new HaskellConsymImpl(node);
      }
      else if (type == CTYPE) {
        return new HaskellCtypeImpl(node);
      }
      else if (type == DATADECL) {
        return new HaskellDatadeclImpl(node);
      }
      else if (type == DEFAULTDECL) {
        return new HaskellDefaultdeclImpl(node);
      }
      else if (type == DERIVINGDECL) {
        return new HaskellDerivingdeclImpl(node);
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
      else if (type == EXPORTSEMPTY) {
        return new HaskellExportsemptyImpl(node);
      }
      else if (type == FIXITY) {
        return new HaskellFixityImpl(node);
      }
      else if (type == FOREIGNDECL) {
        return new HaskellForeigndeclImpl(node);
      }
      else if (type == FUNORPATDECL) {
        return new HaskellFunorpatdeclImpl(node);
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
      else if (type == IMPEMPTY) {
        return new HaskellImpemptyImpl(node);
      }
      else if (type == IMPORTT) {
        return new HaskellImporttImpl(node);
      }
      else if (type == INSTANCEDECL) {
        return new HaskellInstancedeclImpl(node);
      }
      else if (type == KIND) {
        return new HaskellKindImpl(node);
      }
      else if (type == LETEXP) {
        return new HaskellLetexpImpl(node);
      }
      else if (type == LISTLIKE) {
        return new HaskellListlikeImpl(node);
      }
      else if (type == MODULEDECL) {
        return new HaskellModuledeclImpl(node);
      }
      else if (type == NEWCONSTR) {
        return new HaskellNewconstrImpl(node);
      }
      else if (type == NEWTYPEDECL) {
        return new HaskellNewtypedeclImpl(node);
      }
      else if (type == OP) {
        return new HaskellOpImpl(node);
      }
      else if (type == OQTYCON) {
        return new HaskellOqtyconImpl(node);
      }
      else if (type == PAT) {
        return new HaskellPatImpl(node);
      }
      else if (type == PPRAGMA) {
        return new HaskellPpragmaImpl(node);
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
      else if (type == QOP) {
        return new HaskellQopImpl(node);
      }
      else if (type == QQBLOB) {
        return new HaskellQqblobImpl(node);
      }
      else if (type == QTYCLS) {
        return new HaskellQtyclsImpl(node);
      }
      else if (type == QTYCON) {
        return new HaskellQtyconImpl(node);
      }
      else if (type == QTYCONOP) {
        return new HaskellQtyconopImpl(node);
      }
      else if (type == QTYCONSYM) {
        return new HaskellQtyconsymImpl(node);
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
      else if (type == RHS) {
        return new HaskellRhsImpl(node);
      }
      else if (type == SHEBANG) {
        return new HaskellShebangImpl(node);
      }
      else if (type == STMTS) {
        return new HaskellStmtsImpl(node);
      }
      else if (type == TV_BNDR) {
        return new HaskellTvBndrImpl(node);
      }
      else if (type == TYCLS) {
        return new HaskellTyclsImpl(node);
      }
      else if (type == TYCON) {
        return new HaskellTyconImpl(node);
      }
      else if (type == TYCONSYM) {
        return new HaskellTyconsymImpl(node);
      }
      else if (type == TYPEDECL) {
        return new HaskellTypedeclImpl(node);
      }
      else if (type == TYPEE) {
        return new HaskellTypeeImpl(node);
      }
      else if (type == TYVAR) {
        return new HaskellTyvarImpl(node);
      }
      else if (type == VARID) {
        return new HaskellVaridImpl(node);
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
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
