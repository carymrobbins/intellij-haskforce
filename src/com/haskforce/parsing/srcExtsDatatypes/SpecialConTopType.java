package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data SpecialCon l
 *  = UnitCon l             -- ^ unit type and data constructor @()@
 *  | ListCon l             -- ^ list type constructor @[]@
 *  | FunCon  l             -- ^ function type constructor @->@
 *  | TupleCon l Boxed Int  -- ^ /n/-ary tuple type and data
 *  | Cons l                -- ^ list data constructor @(:)@
 *  | UnboxedSingleCon l    -- ^ unboxed singleton tuple constructor @(\# \#)@
 */
public class SpecialConTopType {
}
