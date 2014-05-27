package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * DHead l (Name l) [TyVarBind l]
 */
public class DHead extends DeclHeadTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public TyVarBindTopType[] tyVars;

    @Override
    public String toString() {
        return "DHead{" +
                "name=" + name +
                ", tyVars=" + Arrays.toString(tyVars) +
                '}';
    }
}
