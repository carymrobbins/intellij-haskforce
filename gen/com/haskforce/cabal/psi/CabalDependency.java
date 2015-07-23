// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface CabalDependency extends CabalCompositeElement {

  @NotNull
  CabalDependencyName getDependencyName();

  @NotNull
  List<CabalVersion> getVersionList();

  @NotNull
  List<CabalVersionConstraint> getVersionConstraintList();

}
