package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data ModuleHead l = ModuleHead l (ModuleName l) (Maybe (WarningText l)) (Maybe (ExportSpecList l))
 */
public class ModuleHead {
    public SrcInfoSpan srcInfoSpan;
    public ModuleName moduleName;
    public WarningTextTopType warningTextMaybe;
    public ExportSpecList exportSpecListMaybe;

    @Override
    public String toString() {
        return "ModuleHead{" +
                "moduleName=" + moduleName +
                ", warningTextMaybe=" + warningTextMaybe +
                ", exportSpecListMaybe=" + exportSpecListMaybe +
                '}';
    }
}
