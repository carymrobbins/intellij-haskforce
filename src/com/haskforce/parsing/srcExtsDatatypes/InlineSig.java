package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InlineSig        l Bool (Maybe (Activation l)) (QName l)
 */
public class InlineSig extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public boolean b;
    public ActivationTopType activationMaybe;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "InlineSig{" +
                "qName=" + qName +
                ", activationMaybe=" + activationMaybe +
                ", b=" + b +
                '}';
    }
}
