// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.psi.impl.*;

public interface HaskellTypes {

  IElementType PROPERTY = new HaskellElementType("PROPERTY");

  IElementType COMMENT = new HaskellTokenType("COMMENT");
  IElementType CRLF = new HaskellTokenType("CRLF");
  IElementType KEY = new HaskellTokenType("KEY");
  IElementType SEPARATOR = new HaskellTokenType("SEPARATOR");
  IElementType VALUE = new HaskellTokenType("VALUE");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == PROPERTY) {
        return new SimplePropertyImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
