package com.haskforce.parsing.srcExtsDatatypes;

/**
 * AnnPragma        l (Annotation l)
 */
public class AnnPragma extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public AnnotationTopType annotation;

    @Override
    public String toString() {
        return "AnnPragma{" +
                "annotation=" + annotation +
                '}';
    }
}
