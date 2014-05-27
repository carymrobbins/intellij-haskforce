package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *  TyForall l (Maybe [TyVarBind l]) (Maybe (Context l)) (Type l)
 */
public class TyForall extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TyVarBindTopType[] tyVarBinds;
    public ContextTopType context;
    public TypeTopType type;

    @Override
    public String toString() {
        return "TyForall{" +
                "tyVarBinds=" + Arrays.toString(tyVarBinds) +
                ", context=" + context +
                ", type=" + type +
                '}';
    }
}
