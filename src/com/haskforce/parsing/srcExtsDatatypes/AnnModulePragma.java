package com.haskforce.parsing.srcExtsDatatypes;

/**
 * AnnModulePragma  l (Annotation l)
 */
public class AnnModulePragma extends ModulePragmaTopType {
    public SrcInfoSpan srcInfoSpan;
    public AnnotationTopType ann;

    @Override
    public String toString() {
        return "AnnModulePragma{" +
                "ann=" + ann +
                '}';
    }
}
