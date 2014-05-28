// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;

public interface HaskellQvarid extends PsiNamedElement {

  @Nullable
  HaskellModulePrefix getModulePrefix();

  @NotNull
  HaskellVarid getVarid();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

}
