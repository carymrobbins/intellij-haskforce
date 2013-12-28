// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellConid extends PsiElement {

  @NotNull
  List<HaskellDigit> getDigitList();

  @NotNull
  List<HaskellLarge> getLargeList();

  @NotNull
  List<HaskellSmall> getSmallList();

}
