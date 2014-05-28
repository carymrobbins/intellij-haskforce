package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  Ann       l (Name l)  (Exp l)
 */
public class Ann extends AnnotationTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "Ann{" +
                "name=" + name +
                ", exp=" + exp +
                '}';
    }
}
