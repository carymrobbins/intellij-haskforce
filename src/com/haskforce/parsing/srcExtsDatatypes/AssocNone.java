package com.haskforce.parsing.srcExtsDatatypes;

/**
 * AssocNone  l -- ^ non-associative operator (declared with @infix@)
 */
public class AssocNone extends AssocTopType {
    public SrcInfoSpan srcInfoSpan;

    @Override
    public String toString() {
        return "AssocNone{}";
    }
}
