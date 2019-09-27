package com.haskforce.psi;

import com.intellij.psi.impl.source.tree.injected.StringLiteralEscaper;
import org.jetbrains.annotations.NotNull;

public interface HaskellQqblobBase extends HaskellLanguageInjectionElement {

  @Override
  HaskellQqblob updateText(@NotNull String s);

  @Override
  @NotNull
  StringLiteralEscaper<HaskellQqblob> createLiteralTextEscaper();
}
