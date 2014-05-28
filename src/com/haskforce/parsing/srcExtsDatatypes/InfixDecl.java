package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * InfixDecl    l (Assoc l) (Maybe Int) [Op l]
 */
public class InfixDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public AssocTopType assoc;
    public Integer intMaybe;
    public OpTopType[] ops;

    @Override
    public String toString() {
        return "InfixDecl{" +
                "ops=" + Arrays.toString(ops) +
                ", intMaybe=" + intMaybe +
                ", assoc=" + assoc +
                '}';
    }
}
