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

public class CabalKeyImpl extends CabalCompositeElementImpl implements CabalKey {

  public CabalKeyImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitKey(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public CabalAuthor getAuthor() {
    return findChildByClass(CabalAuthor.class);
  }

  @Override
  @Nullable
  public CabalBugReports getBugReports() {
    return findChildByClass(CabalBugReports.class);
  }

  @Override
  @Nullable
  public CabalBuildType getBuildType() {
    return findChildByClass(CabalBuildType.class);
  }

  @Override
  @Nullable
  public CabalCabalPackage getCabalPackage() {
    return findChildByClass(CabalCabalPackage.class);
  }

  @Override
  @Nullable
  public CabalCabalVersion getCabalVersion() {
    return findChildByClass(CabalCabalVersion.class);
  }

  @Override
  @Nullable
  public CabalCategory getCategory() {
    return findChildByClass(CabalCategory.class);
  }

  @Override
  @Nullable
  public CabalCopyright getCopyright() {
    return findChildByClass(CabalCopyright.class);
  }

  @Override
  @Nullable
  public CabalDataDir getDataDir() {
    return findChildByClass(CabalDataDir.class);
  }

  @Override
  @Nullable
  public CabalDataFiles getDataFiles() {
    return findChildByClass(CabalDataFiles.class);
  }

  @Override
  @Nullable
  public CabalDescription getDescription() {
    return findChildByClass(CabalDescription.class);
  }

  @Override
  @Nullable
  public CabalExtraDocFiles getExtraDocFiles() {
    return findChildByClass(CabalExtraDocFiles.class);
  }

  @Override
  @Nullable
  public CabalExtraSourceFiles getExtraSourceFiles() {
    return findChildByClass(CabalExtraSourceFiles.class);
  }

  @Override
  @Nullable
  public CabalExtraTmpFiles getExtraTmpFiles() {
    return findChildByClass(CabalExtraTmpFiles.class);
  }

  @Override
  @Nullable
  public CabalHomepage getHomepage() {
    return findChildByClass(CabalHomepage.class);
  }

  @Override
  @Nullable
  public CabalLicense getLicense() {
    return findChildByClass(CabalLicense.class);
  }

  @Override
  @Nullable
  public CabalLicenseFile getLicenseFile() {
    return findChildByClass(CabalLicenseFile.class);
  }

  @Override
  @Nullable
  public CabalLicenseFiles getLicenseFiles() {
    return findChildByClass(CabalLicenseFiles.class);
  }

  @Override
  @Nullable
  public CabalMaintainer getMaintainer() {
    return findChildByClass(CabalMaintainer.class);
  }

  @Override
  @Nullable
  public CabalOtherExtensions getOtherExtensions() {
    return findChildByClass(CabalOtherExtensions.class);
  }

  @Override
  @Nullable
  public CabalPackageVersion getPackageVersion() {
    return findChildByClass(CabalPackageVersion.class);
  }

  @Override
  @Nullable
  public CabalProjectName getProjectName() {
    return findChildByClass(CabalProjectName.class);
  }

  @Override
  @Nullable
  public CabalStability getStability() {
    return findChildByClass(CabalStability.class);
  }

  @Override
  @Nullable
  public CabalSynopsis getSynopsis() {
    return findChildByClass(CabalSynopsis.class);
  }

  @Override
  @Nullable
  public CabalTestedWith getTestedWith() {
    return findChildByClass(CabalTestedWith.class);
  }

}
