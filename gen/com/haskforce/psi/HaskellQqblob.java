// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.injected.StringLiteralEscaper;

public interface HaskellQqblob extends HaskellLanguageInjectionElement {

  boolean isValidHost();

  HaskellQqblob updateText(@NotNull String s);

  @NotNull
  StringLiteralEscaper<HaskellQqblob> createLiteralTextEscaper();

}
