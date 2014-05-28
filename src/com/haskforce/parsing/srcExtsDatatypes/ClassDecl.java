package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * ClassDecl    l (Maybe (Context l)) (DeclHead l) [FunDep l] (Maybe [ClassDecl l])
 */
public class ClassDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ContextTopType contextMaybe;
    public DeclHeadTopType declHead;
    public FunDep[] funDeps;
    public ClassDecl[] classDecls;

    @Override
    public String toString() {
        return "ClassDecl{" +
                "classDecls=" + Arrays.toString(classDecls) +
                ", funDeps=" + Arrays.toString(funDeps) +
                ", declHead=" + declHead +
                ", contextMaybe=" + contextMaybe +
                '}';
    }
}
