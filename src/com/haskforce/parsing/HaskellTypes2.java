package com.haskforce.parsing;

import com.haskforce.psi.HaskellElementType;
import com.haskforce.psi.HaskellTypes;
import com.haskforce.psi.impl.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

/**
 * Placeholder for elements and tokens for the new parser.
 */
public interface HaskellTypes2 {

    IElementType BODY = new HaskellElementType("BODY");
    IElementType CNAME = new HaskellElementType("CNAME");
    IElementType CNAMES = new HaskellElementType("CNAMES");
    IElementType CON = new HaskellElementType("CON");
    IElementType CONSYM = new HaskellElementType("CONSYM");
    IElementType CPP = new HaskellElementType("CPP");
    IElementType EXPORT = new HaskellElementType("EXPORT");
    IElementType EXPORTS = new HaskellElementType("EXPORTS");
    IElementType IMPDECL = new HaskellElementType("IMPDECL");
    IElementType IMPORTT = new HaskellElementType("IMPORTT");
    IElementType MODULE = new HaskellElementType("MODULE");
    IElementType NAME = new HaskellElementType("NAME");
    IElementType NCOMMENT = new HaskellElementType("NCOMMENT");
    IElementType PPRAGMA = new HaskellElementType("PPRAGMA");
    IElementType PSTRINGTOKEN = new HaskellElementType("PSTRINGTOKEN");
    IElementType QCONID = new HaskellElementType("QCONID");
    IElementType QCONSYM = new HaskellElementType("QCONSYM");
    IElementType QINFIXCONID = new HaskellElementType("QINFIXCONID");
    IElementType QINFIXVARID = new HaskellElementType("QINFIXVARID");
    IElementType QTYCLS = new HaskellElementType("QTYCLS");
    IElementType QTYCON = new HaskellElementType("QTYCON");
    IElementType QVAR = new HaskellElementType("QVAR");
    IElementType QVARID = new HaskellElementType("QVARID");
    IElementType QVARS = new HaskellElementType("QVARS");
    IElementType QVARSYM = new HaskellElementType("QVARSYM");
    IElementType RESERVEDOP = new HaskellElementType("RESERVEDOP");
    IElementType SPECIAL = new HaskellElementType("SPECIAL");
    IElementType SYMBOL = new HaskellElementType("SYMBOL");
    IElementType TYCLS = new HaskellElementType("TYCLS");
    IElementType TYCON = new HaskellElementType("TYCON");
    IElementType TYVAR = new HaskellElementType("TYVAR");
    IElementType VARID = new HaskellElementType("VARID");
    IElementType VARS = new HaskellElementType("VARS");
    IElementType VARSYM = new HaskellElementType("VARSYM");
    IElementType WHITECHAR = new HaskellElementType("WHITECHAR");

    class Factory {
        /**
         * Called when the parser marks an element.
         */
        public static PsiElement createElement(ASTNode node) {
            IElementType type = node.getElementType();
            if (type == BODY) {
                return new HaskellBodyImpl(node);
            }
            else if (type == CNAME) {
                return new HaskellCnameImpl(node);
            }
            else if (type == CNAMES) {
                return new HaskellCnamesImpl(node);
            }
            else if (type == CON || type == HaskellTypes.CONID) {
                return new HaskellConImpl(node);
            }
            else if (type == CONSYM) {
                return new HaskellConsymImpl(node);
            }
            else if (type == CPP) {
                return new HaskellCppImpl(node);
            }
            else if (type == EXPORT) {
                return new HaskellExportImpl(node);
            }
            else if (type == EXPORTS) {
                return new HaskellExportsImpl(node);
            }
            else if (type == IMPDECL) {
                return new HaskellImpdeclImpl(node);
            }
            else if (type == IMPORTT || type == HaskellTypes.IMPORT) {
                return new HaskellImporttImpl(node);
            }
            else if (type == HaskellTypes.MODULE) {
                return new HaskellModulePrefixImpl(node);
            }
            else if (type == NCOMMENT || type == HaskellTypes.OPENCOM) {
                return new HaskellNcommentImpl(node);
            }
            else if (type == PPRAGMA || type == HaskellTypes.OPENPRAGMA) {
                return new HaskellPpragmaImpl(node);
            }
            else if (type == PSTRINGTOKEN || type == HaskellTypes.DOUBLEQUOTE) {
                return new HaskellPstringtokenImpl(node);
            }
            else if (type == QCONID) {
                return new HaskellQconidImpl(node);
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
            else if (type == QTYCLS) {
                return new HaskellQtyclsImpl(node);
            }
            else if (type == QTYCON) {
                return new HaskellQtyconImpl(node);
            }
            else if (type == QVAR) {
                return new HaskellQvarImpl(node);
            }
            else if (type == QVARID) {
                return new HaskellQvaridImpl(node);
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
            else if (type == SPECIAL) {
                return new HaskellSpecialImpl(node);
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
            else if (type == TYVAR) {
                return new HaskellTyvarImpl(node);
            }
            else if (type == VARID || type == HaskellTypes.VARIDREGEXP) {
                return new HaskellVaridImpl(node);
            }
            else if (type == VARS) {
                return new HaskellVarsImpl(node);
            }
            else if (type == VARSYM) {
                return new HaskellVarsymImpl(node);
            }
            else if (type == WHITECHAR) {
                return new HaskellWhitecharImpl(node);
            } else {
                return new HaskellCompositeElementType(node);
            }
        }
    }
}
