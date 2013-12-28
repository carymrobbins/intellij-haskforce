// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellLexeme extends PsiElement {

  @Nullable
  HaskellLiteral getLiteral();

  @Nullable
  HaskellQconid getQconid();

  @Nullable
  HaskellQconsym getQconsym();

  @Nullable
  HaskellQvarid getQvarid();

  @Nullable
  HaskellQvarsym getQvarsym();

  @Nullable
  HaskellReservedid getReservedid();

  @Nullable
  HaskellReservedop getReservedop();

  @Nullable
  HaskellSpecial getSpecial();

}
