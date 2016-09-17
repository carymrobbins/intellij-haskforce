package com.haskforce.tools.yesod.shakespeare.hamlet.psi;

import com.intellij.psi.tree.IElementType;

public interface HamletTypes {
    IElementType CLASS_ATTRIBUTE = new HamletTokenType("CLASS_ATTRIBUTE");
    IElementType ID_ATTRIBUTE = new HamletTokenType("ID_ATTRIBUTE");
    IElementType LOGIC = new HamletTokenType("LOGIC");
    IElementType HASKELL_CODE = new HamletTokenType("HASKELL_CODE");
    IElementType LINE_COMMENT = new HamletTokenType("LINE_COMMENT");
    IElementType HASKELL_INTERPOLATE_OPEN = new HamletTokenType("#{");
    IElementType ROUTE_INTERPOLATE_OPEN = new HamletTokenType("@{");
    IElementType WIDGET_INTERPOLATE_OPEN = new HamletTokenType("^{");
    IElementType ATTR_INTERPOLATE_OPEN = new HamletTokenType("*{");
    IElementType LANG_INTERPOLATE_OPEN = new HamletTokenType("_{");
    IElementType INTERPOLATE_CLOSE = new HamletTokenType("}");
}
