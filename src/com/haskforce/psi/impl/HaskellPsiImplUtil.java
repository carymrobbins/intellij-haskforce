package com.haskforce.psi.impl;

import com.intellij.lang.ASTNode;
import com.haskforce.psi.HaskellProperty;
import com.haskforce.psi.HaskellTypes;

public class HaskellPsiImplUtil {
    public static String getKey(HaskellProperty element) {
        ASTNode keyNode = element.getNode().findChildByType(HaskellTypes.KEY);
        if (keyNode != null) {
            return keyNode.getText();
        } else {
            return null;
        }
    }

    public static String getValue(HaskellProperty element) {
        ASTNode valueNode = element.getNode().findChildByType(HaskellTypes.VALUE);
        if (valueNode != null) {
            return valueNode.getText();
        } else {
            return null;
        }
    }
}
