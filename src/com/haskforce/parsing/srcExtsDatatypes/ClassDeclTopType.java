package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data ClassDecl l
 *  = ClsDecl    l (Decl l) -- ^ ordinary declaration
 *  | ClsDataFam l (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) -- ^ declaration of an associated data type
 *  | ClsTyFam   l                     (DeclHead l) (Maybe (Kind l)) -- ^ declaration of an associated type synonym
 *  | ClsTyDef   l (Type l) (Type l)  -- ^ default choice for an associated type synonym
 */
public class ClassDeclTopType {
}
