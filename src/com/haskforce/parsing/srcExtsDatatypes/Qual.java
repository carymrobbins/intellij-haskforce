package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Qual    l (ModuleName l) (Name l)
 */
public class Qual extends QNameTopType {
    public SrcInfoSpan srcInfoSpan;
    public ModuleName moduleName;
    public NameTopType name;

    @Override
    public String toString() {
        return "Qual{" +
                "name=" + name +
                ", moduleName=" + moduleName +
                '}';
    }
}
