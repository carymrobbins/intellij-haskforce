package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data RPat l
 *  = RPOp l (RPat l) (RPatOp l)   -- ^ operator pattern, e.g. pat*
 *  | RPEither l (RPat l) (RPat l) -- ^ choice pattern, e.g. (1 | 2)
 *  | RPSeq l [RPat l]             -- ^ sequence pattern, e.g. (| 1, 2, 3 |)
 *  | RPGuard l (Pat l) [Stmt l]   -- ^ guarded pattern, e.g. (| p | p < 3 |)
 *  | RPCAs l (Name l) (RPat l)    -- ^ non-linear variable binding, e.g. (foo\@:(1 | 2))*
 *  | RPAs l (Name l) (RPat l)     -- ^ linear variable binding, e.g. foo\@(1 | 2)
 *  | RPParen l (RPat l)           -- ^ parenthesised pattern, e.g. (2*)
 *  | RPPat l (Pat l)              -- ^ an ordinary pattern
 */
public class RPatTopType {
}
