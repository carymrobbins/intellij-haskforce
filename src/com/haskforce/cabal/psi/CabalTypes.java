package com.haskforce.cabal.psi;

import com.intellij.psi.tree.IElementType;

public interface CabalTypes {
    IElementType COMMENT = new CabalTokenType("COMMENT");
    IElementType KEY = new CabalElementType("KEY");
    IElementType COLON = new CabalElementType("COLON");
    IElementType VALUE_CHAR = new CabalTokenType("VALUE_CHAR");
    IElementType CRLF = new CabalTokenType("CRLF");
    IElementType CONFIG = new CabalElementType("CONFIG");
    IElementType CONDITIONAL = new CabalElementType("CONDITIONAL");
}
