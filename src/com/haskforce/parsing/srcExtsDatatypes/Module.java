package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
 */
public class Module extends ModuleTopType {
    public SrcInfoSpan srcInfoSpan;
    public ModuleHead moduleHeadMaybe;
    public ModulePragmaTopType[] modulePragmas;
    public ImportDecl[] importDecls;
    public DeclTopType[] decls;

    @Override
    public String toString() {
        return "Module{" +
                "decls=" + Arrays.toString(decls) +
                ", importDecls=" + Arrays.toString(importDecls) +
                ", modulePragmas=" + Arrays.toString(modulePragmas) +
                ", moduleHeadMaybe=" + moduleHeadMaybe +
                '}';
    }
}
