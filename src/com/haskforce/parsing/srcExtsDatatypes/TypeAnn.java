package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TypeAnn   l (Name l)  (Exp l)
 */
public class TypeAnn extends AnnotationTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "TypeAnn{" +
                "name=" + name +
                ", exp=" + exp +
                '}';
    }
}
