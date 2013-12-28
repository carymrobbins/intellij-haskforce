// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellEscape extends PsiElement {

  @Nullable
  HaskellDecimal getDecimal();

  @Nullable
  HaskellHexadecimal getHexadecimal();

  @Nullable
  HaskellOctal getOctal();

  @Nullable
  PsiElement getAscii();

  @Nullable
  PsiElement getCharesc();

}
