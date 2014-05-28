package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TypeInsDecl  l (Type l) (Type l)
 */
public class TypeInsDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "TypeInsDecl{" +
                "t1=" + t1 +
                ", t2=" + t2 +
                '}';
    }
}
