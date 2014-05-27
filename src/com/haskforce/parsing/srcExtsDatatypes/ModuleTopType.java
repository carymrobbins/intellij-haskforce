package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Module l
 *  = Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
 *  | XmlPage l (ModuleName l) [ModulePragma l] (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
 *  | XmlHybrid l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
 *              (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
 */
public class ModuleTopType {
}
