package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * XmlHybrid l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
 (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
 */
public class XmlHybrid extends ModuleTopType {
    public SrcInfoSpan srcInfoSpan;
    public ModuleHead moduleHeadMaybe;
    public ModulePragmaTopType[] modulePragmas;
    public ImportDecl[] importDecls;
    public DeclTopType[] decls;
    public XName xName;
    public XAttr[] xAttrs;
    public ExpTopType expMaybe;
    public ExpTopType[] exps;

    @Override
    public String toString() {
        return "XmlHybrid{" +
                "exps=" + Arrays.toString(exps) +
                ", expMaybe=" + expMaybe +
                ", xAttrs=" + Arrays.toString(xAttrs) +
                ", xName=" + xName +
                ", decls=" + Arrays.toString(decls) +
                ", importDecls=" + Arrays.toString(importDecls) +
                ", modulePragmas=" + Arrays.toString(modulePragmas) +
                ", moduleHeadMaybe=" + moduleHeadMaybe +
                '}';
    }
}
