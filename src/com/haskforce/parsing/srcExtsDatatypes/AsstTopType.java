package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Asst l
 *  = ClassA l (QName l) [Type l]           -- ^ ordinary class assertion
 *  | InfixA l (Type l) (QName l) (Type l)  -- ^ class assertion where the class name is given infix
 *  | IParam l (IPName l) (Type l)          -- ^ implicit parameter assertion
 *  | EqualP l (Type l) (Type l)            -- ^ type equality constraint
 */
public class AsstTopType {
}
