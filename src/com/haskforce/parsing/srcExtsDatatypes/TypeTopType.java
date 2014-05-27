package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Type l
 *  = TyForall l
 *    (Maybe [TyVarBind l])
 *    (Maybe (Context l))
 *    (Type l)                                -- ^ qualified type
 *  | TyFun   l (Type l) (Type l)              -- ^ function type
 *  | TyTuple l Boxed [Type l]                 -- ^ tuple type, possibly boxed
 *  | TyList  l (Type l)                       -- ^ list syntax, e.g. [a], as opposed to [] a
 *  | TyApp   l (Type l) (Type l)              -- ^ application of a type constructor
 *  | TyVar   l (Name l)                       -- ^ type variable
 *  | TyCon   l (QName l)                      -- ^ named type or type constructor
 *  | TyParen l (Type l)                       -- ^ type surrounded by parentheses
 *  | TyInfix l (Type l) (QName l) (Type l)    -- ^ infix type constructor
 *  | TyKind  l (Type l) (Kind l)              -- ^ type with explicit kind signature
 *  | TyPromoted l (Promoted l)                -- ^ @'K@, a promoted data type (-XDataKinds).
 */
public class TypeTopType {
}
