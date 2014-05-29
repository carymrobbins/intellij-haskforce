package com.haskforce.parsing.srcExtsDatatypes;

/**
 * EModuleContents l (ModuleName l)
 */
public class EModuleContents extends ExportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public ModuleName moduleName;

    @Override
    public String toString() {
        return "EModuleContents{" +
                "moduleName=" + moduleName +
                '}';
    }
}
