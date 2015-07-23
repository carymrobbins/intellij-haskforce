package com.haskforce.cabal.psi;

import com.intellij.navigation.NavigationItem;
import com.intellij.psi.PsiNameIdentifierOwner;

public interface CabalNamedElement extends CabalCompositeElement , PsiNameIdentifierOwner, NavigationItem {
}
