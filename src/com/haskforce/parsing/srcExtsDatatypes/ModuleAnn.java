package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ModuleAnn l           (Exp l)
 */
public class ModuleAnn extends AnnotationTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "ModuleAnn{" +
                "exp=" + exp +
                '}';
    }
}
