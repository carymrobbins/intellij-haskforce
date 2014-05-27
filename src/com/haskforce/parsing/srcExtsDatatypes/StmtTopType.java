package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Stmt l
 *  = Generator l (Pat l) (Exp l)
 *  | Qualifier l (Exp l)   -- ^ an /exp/ by itself: in a @do@-expression,
 *  | LetStmt l (Binds l)   -- ^ local bindings
 *  | RecStmt l [Stmt l]    -- ^ a recursive binding group for arrows
 */
public class StmtTopType {
}
