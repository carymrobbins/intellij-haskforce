package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data ExportSpec l
 *  = EVar l (QName l)                 -- ^ variable
 *  | EAbs l (QName l)                 -- ^ @T@:
 *  | EThingAll l (QName l)            -- ^ @T(..)@:
 *  | EThingWith l (QName l) [CName l] -- ^ @T(C_1,...,C_n)@:
 *  | EModuleContents l (ModuleName l) -- ^ @module M@:
 */
public class ExportSpecTopType { // TODO: Complete me.
}
