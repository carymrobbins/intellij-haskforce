package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * SpecSig          l      (Maybe (Activation l)) (QName l) [Type l]
 */
public class SpecSig extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ActivationTopType activationMaybe;
    public QNameTopType qName;
    public TypeTopType[] types;

    @Override
    public String toString() {
        return "SpecSig{" +
                "types=" + Arrays.toString(types) +
                ", qName=" + qName +
                ", activationMaybe=" + activationMaybe +
                '}';
    }
}
