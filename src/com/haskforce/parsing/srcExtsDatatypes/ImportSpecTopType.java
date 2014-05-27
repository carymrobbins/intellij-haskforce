package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data ImportSpec l
 *  = IVar l (Name l)                  -- ^ variable
 *  | IAbs l (Name l)                  -- ^ @T@:
 *   --   the name of a class, datatype or type synonym.
 *  | IThingAll l (Name l)             -- ^ @T(..)@:
 *  --   a class imported with all of its methods, or
 *  --   a datatype imported with all of its constructors.
 *  | IThingWith l (Name l) [CName l]  -- ^ @T(C_1,...,C_n)@:
 *  --   a class imported with some of its methods, or
 *  --   a datatype imported with some of its constructors.
 */
public class ImportSpecTopType {
    // TODO
}


