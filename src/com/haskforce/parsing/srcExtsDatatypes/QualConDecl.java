package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * data QualConDecl l
 *  = QualConDecl l
 *   {-forall-} (Maybe [TyVarBind l]) {- . -} (Maybe (Context l))
 *   {- => -} (ConDecl l)
 */
public class QualConDecl { // TODO: Deserialize.
    public SrcInfoSpan srcInfoSpan;
    public TyVarBindTopType[] tyVarBinds;
    public ContextTopType contextMaybe;
    public ConDeclTopType conDecl;

    @Override
    public String toString() {
        return "QualConDecl{" +
                "tyVarBinds=" + Arrays.toString(tyVarBinds) +
                ", contextMaybe=" + contextMaybe +
                ", conDecl=" + conDecl +
                '}';
    }
}
