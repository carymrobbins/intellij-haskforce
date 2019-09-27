package com.haskforce.psi.impl;

import com.haskforce.psi.HaskellQqblob;
import com.intellij.lang.ASTNode;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.injected.StringLiteralEscaper;
import org.jetbrains.annotations.NotNull;

abstract class HaskellQqblobBaseImpl
  extends HaskellCompositeElementImpl
  implements HaskellQqblob {

  HaskellQqblobBaseImpl(ASTNode node) {
    super(node);
  }

  @Override
  public boolean isValidHost() {
    return true;
  }

  @Override
  public HaskellQqblob updateText(@NotNull String s) {
    final ASTNode valueNode = getNode().getFirstChildNode();
    assert valueNode instanceof LeafElement;
    ((LeafElement) valueNode).replaceWithText(s);
    return this;
  }

  @Override
  @NotNull
  public StringLiteralEscaper<HaskellQqblob> createLiteralTextEscaper() {
    return new StringLiteralEscaper<>(this);
  }
}
