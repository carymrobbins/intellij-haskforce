package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyPromoted l (Promoted l)
 */
public class TyPromoted extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public PromotedTopType promoted;

    @Override
    public String toString() {
        return "TyPromoted{" +
                "promoted=" + promoted +
                '}';
    }
}
