package com.haskforce.parsing.srcExtsDatatypes;

/**
 *data ImportDecl l = ImportDecl
 { importAnn :: l                   -- ^ annotation, used by parser for position of the @import@ keyword.
 , importModule :: (ModuleName l)   -- ^ name of the module imported.
 , importQualified :: Bool          -- ^ imported @qualified@?
 , importSrc :: Bool                -- ^ imported with @{-\# SOURCE \#-}@?
 , importPkg :: Maybe String        -- ^ imported with explicit package name
 , importAs :: Maybe (ModuleName l) -- ^ optional alias name in an @as@ clause.
 , importSpecs :: Maybe (ImportSpecList l)
 }
 */
public class ImportDecl {
    public SrcInfoSpan importAnn;
    public ModuleName importModule;
    public boolean importQualified;
    public boolean importSrc;
    public String importPkg;
    public ModuleName importAs;
    public ImportSpecList importSpecs;

    @Override
    public String toString() {
        return "ImportDecl{" +
                "importModule=" + importModule +
                ", importQualified=" + importQualified +
                ", importSrc=" + importSrc +
                ", importPkg='" + importPkg + '\'' +
                ", importAs=" + importAs +
                ", importSpecs=" + importSpecs +
                '}';
    }
}
