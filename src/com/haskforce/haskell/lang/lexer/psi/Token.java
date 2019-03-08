package com.haskforce.haskell.lang.lexer.psi;

import com.haskforce.HaskellLanguage;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class Token extends IElementType {
  public Token(@NotNull String debugName) {
    super(debugName, HaskellLanguage.INSTANCE);
  }
}
