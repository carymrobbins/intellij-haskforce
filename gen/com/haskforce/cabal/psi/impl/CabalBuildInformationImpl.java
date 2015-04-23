// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.cabal.psi.CabalTypes.*;
import com.haskforce.cabal.psi.*;

public class CabalBuildInformationImpl extends CabalCompositeElementImpl implements CabalBuildInformation {

  public CabalBuildInformationImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitBuildInformation(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public CabalBuildDepends getBuildDepends() {
    return findChildByClass(CabalBuildDepends.class);
  }

  @Override
  @Nullable
  public CabalBuildTools getBuildTools() {
    return findChildByClass(CabalBuildTools.class);
  }

  @Override
  @Nullable
  public CabalBuildable getBuildable() {
    return findChildByClass(CabalBuildable.class);
  }

  @Override
  @Nullable
  public CabalCSources getCSources() {
    return findChildByClass(CabalCSources.class);
  }

  @Override
  @Nullable
  public CabalCcOptions getCcOptions() {
    return findChildByClass(CabalCcOptions.class);
  }

  @Override
  @Nullable
  public CabalCppOptions getCppOptions() {
    return findChildByClass(CabalCppOptions.class);
  }

  @Override
  @Nullable
  public CabalDefaultLanguage getDefaultLanguage() {
    return findChildByClass(CabalDefaultLanguage.class);
  }

  @Override
  @Nullable
  public CabalExtensions getExtensions() {
    return findChildByClass(CabalExtensions.class);
  }

  @Override
  @Nullable
  public CabalExtraGhciLibraries getExtraGhciLibraries() {
    return findChildByClass(CabalExtraGhciLibraries.class);
  }

  @Override
  @Nullable
  public CabalExtraLibDirs getExtraLibDirs() {
    return findChildByClass(CabalExtraLibDirs.class);
  }

  @Override
  @Nullable
  public CabalExtraLibraries getExtraLibraries() {
    return findChildByClass(CabalExtraLibraries.class);
  }

  @Override
  @Nullable
  public CabalFrameworks getFrameworks() {
    return findChildByClass(CabalFrameworks.class);
  }

  @Override
  @Nullable
  public CabalGhcOptions getGhcOptions() {
    return findChildByClass(CabalGhcOptions.class);
  }

  @Override
  @Nullable
  public CabalGhcProfOptions getGhcProfOptions() {
    return findChildByClass(CabalGhcProfOptions.class);
  }

  @Override
  @Nullable
  public CabalGhcSharedOptions getGhcSharedOptions() {
    return findChildByClass(CabalGhcSharedOptions.class);
  }

  @Override
  @Nullable
  public CabalHsSourceDirs getHsSourceDirs() {
    return findChildByClass(CabalHsSourceDirs.class);
  }

  @Override
  @Nullable
  public CabalIncludeDirs getIncludeDirs() {
    return findChildByClass(CabalIncludeDirs.class);
  }

  @Override
  @Nullable
  public CabalIncludes getIncludes() {
    return findChildByClass(CabalIncludes.class);
  }

  @Override
  @Nullable
  public CabalInstallIncludes getInstallIncludes() {
    return findChildByClass(CabalInstallIncludes.class);
  }

  @Override
  @Nullable
  public CabalJsSources getJsSources() {
    return findChildByClass(CabalJsSources.class);
  }

  @Override
  @Nullable
  public CabalLdOptions getLdOptions() {
    return findChildByClass(CabalLdOptions.class);
  }

  @Override
  @Nullable
  public CabalOtherModules getOtherModules() {
    return findChildByClass(CabalOtherModules.class);
  }

  @Override
  @Nullable
  public CabalPkgConfigDepends getPkgConfigDepends() {
    return findChildByClass(CabalPkgConfigDepends.class);
  }

}
