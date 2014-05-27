package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Promoted l
 *  = PromotedInteger l Integer String -- ^ parsed value and raw string
 *  | PromotedString l String String -- ^ parsed value and raw string
 *  | PromotedCon l Bool (QName l)
 *  | PromotedList l Bool [Promoted l]
 *  | PromotedTuple l [Promoted l]
 *  | PromotedUnit l
 */
public class PromotedTopType { // TODO: Make rest of Promoted*
}
