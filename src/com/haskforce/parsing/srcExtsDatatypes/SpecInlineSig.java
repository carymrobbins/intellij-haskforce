package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * SpecInlineSig    l Bool (Maybe (Activation l)) (QName l) [Type l]
 */
public class SpecInlineSig extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public boolean b;
    public ActivationTopType activationMaybe;
    public QNameTopType qName;
    public TypeTopType[] types;

    @Override
    public String toString() {
        return "SpecInlineSig{" +
                "types=" + Arrays.toString(types) +
                ", qName=" + qName +
                ", activationMaybe=" + activationMaybe +
                ", b=" + b +
                '}';
    }
}
