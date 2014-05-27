package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data InstDecl l
 *  = InsDecl   l (Decl l)
 *  | InsType   l (Type l) (Type l)
 *  | InsData   l (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l))
 *  | InsGData  l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l))
 */
public class InstDeclTopType {
}
