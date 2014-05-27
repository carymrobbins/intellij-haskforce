package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Bracket l
 *  = ExpBracket l (Exp l)        -- ^ expression bracket: @[| ... |]@
 *  | PatBracket l (Pat l)        -- ^ pattern bracket: @[p| ... |]@
 *  | TypeBracket l (Type l)      -- ^ type bracket: @[t| ... |]@
 *  | DeclBracket l [Decl l]      -- ^ declaration bracket: @[d| ... |]@
 */
public class BracketTopType {
}
