// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.cabal.psi.impl.*;

public interface CabalTypes {

  IElementType COMPLEXKEY = new CabalElementType("COMPLEXKEY");
  IElementType COMPLEXKEYNAME = new CabalElementType("COMPLEXKEYNAME");
  IElementType CONDITIONAL = new CabalElementType("CONDITIONAL");
  IElementType CONFIG = new CabalElementType("CONFIG");
  IElementType DEPENDENCY = new CabalElementType("DEPENDENCY");
  IElementType DEPENDENCY_NAME = new CabalElementType("DEPENDENCY_NAME");
  IElementType EXECUTABLE = new CabalElementType("EXECUTABLE");
  IElementType FLAG = new CabalElementType("FLAG");
  IElementType KEY = new CabalElementType("KEY");
  IElementType KEY_OR_CONFIG = new CabalElementType("KEY_OR_CONFIG");
  IElementType LIBRARY = new CabalElementType("LIBRARY");
  IElementType MODULE = new CabalElementType("MODULE");
  IElementType NUMBER = new CabalElementType("NUMBER");
  IElementType SIMPLEKEY = new CabalElementType("SIMPLEKEY");
  IElementType SIMPLEKEYNAME = new CabalElementType("SIMPLEKEYNAME");
  IElementType VARID = new CabalElementType("VARID");
  IElementType VERSION = new CabalElementType("VERSION");
  IElementType VERSION_CONSTRAINT = new CabalElementType("VERSION_CONSTRAINT");

  IElementType COLON = new CabalTokenType(":");
  IElementType COMMA = new CabalTokenType(",");
  IElementType COMMENT = new CabalTokenType("comment");
  IElementType DOT = new CabalTokenType(".");
  IElementType NUMBERREGEXP = new CabalTokenType("numberRegexp");
  IElementType VARIDREGEXP = new CabalTokenType("varidRegexp");
  IElementType WHITESPACELBRACETOK = new CabalTokenType("Synthetic leftbrace");
  IElementType WHITESPACERBRACETOK = new CabalTokenType("Synthetic rightbrace");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == COMPLEXKEY) {
        return new CabalComplexkeyImpl(node);
      }
      else if (type == COMPLEXKEYNAME) {
        return new CabalComplexkeynameImpl(node);
      }
      else if (type == CONDITIONAL) {
        return new CabalConditionalImpl(node);
      }
      else if (type == CONFIG) {
        return new CabalConfigImpl(node);
      }
      else if (type == DEPENDENCY) {
        return new CabalDependencyImpl(node);
      }
      else if (type == DEPENDENCY_NAME) {
        return new CabalDependencyNameImpl(node);
      }
      else if (type == EXECUTABLE) {
        return new CabalExecutableImpl(node);
      }
      else if (type == FLAG) {
        return new CabalFlagImpl(node);
      }
      else if (type == KEY) {
        return new CabalKeyImpl(node);
      }
      else if (type == KEY_OR_CONFIG) {
        return new CabalKeyOrConfigImpl(node);
      }
      else if (type == LIBRARY) {
        return new CabalLibraryImpl(node);
      }
      else if (type == MODULE) {
        return new CabalModuleImpl(node);
      }
      else if (type == NUMBER) {
        return new CabalNumberImpl(node);
      }
      else if (type == SIMPLEKEY) {
        return new CabalSimplekeyImpl(node);
      }
      else if (type == SIMPLEKEYNAME) {
        return new CabalSimplekeynameImpl(node);
      }
      else if (type == VARID) {
        return new CabalVaridImpl(node);
      }
      else if (type == VERSION) {
        return new CabalVersionImpl(node);
      }
      else if (type == VERSION_CONSTRAINT) {
        return new CabalVersionConstraintImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
