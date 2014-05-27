package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data QName l
 *  = Qual    l (ModuleName l) (Name l) -- ^ name qualified with a module name
 *  | UnQual  l                (Name l) -- ^ unqualified local name
 *  | Special l (SpecialCon l)          -- ^ built-in constructor with special syntax
 */
public class QNameTopType {
}
