package com.haskforce.haskell.lang.lexer.psi;

import com.haskforce.HaskellLanguage;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class LexerToken extends IElementType {
  public LexerToken(@NotNull String debugName) {
    super(debugName, HaskellLanguage.INSTANCE);
  }
}
