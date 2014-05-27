package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Kind l
 *  = KindStar  l                    -- ^ @*@, the kind of types
 *  | KindBang  l                    -- ^ @!@, the kind of unboxed types
 *  | KindFn    l (Kind l) (Kind l)  -- ^ @->@, the kind of a type constructor
 *  | KindParen l (Kind l)           -- ^ a parenthesised kind
 *  | KindVar   l (QName l)          -- ^ @k@, a kind variable (-XPolyKinds)
 *  | KindApp   l (Kind l) (Kind l)  -- ^ @k1 k2@
 *  | KindTuple l [Kind l]           -- ^ @'(k1,k2,k3)@, a promoted tuple
 *  | KindList  l [Kind l]           -- ^ @'[k1,k2,k3]@, a promoted list literal
 */
public class KindTopType {
}
