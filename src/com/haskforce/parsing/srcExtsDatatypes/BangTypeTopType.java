package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data BangType l
 *  = BangedTy   l (Type l) -- ^ strict component, marked with \"@!@\"
 *  | UnBangedTy l (Type l) -- ^ non-strict component
 *  | UnpackedTy l (Type l) -- ^ unboxed component, marked with an UNPACK pragma
 */
public class BangTypeTopType {
}
