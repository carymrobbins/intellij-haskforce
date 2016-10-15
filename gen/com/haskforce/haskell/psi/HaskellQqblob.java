// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.impl.source.tree.injected.StringLiteralEscaper;

public interface HaskellQqblob extends PsiLanguageInjectionHost {

  boolean isValidHost();

  HaskellQqblob updateText(String s);

  @NotNull
  StringLiteralEscaper<HaskellQqblob> createLiteralTextEscaper();

}
