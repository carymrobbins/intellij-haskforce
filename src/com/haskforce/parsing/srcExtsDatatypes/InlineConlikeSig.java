package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InlineConlikeSig l      (Maybe (Activation l)) (QName l)
 */
public class InlineConlikeSig extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ActivationTopType activationMaybe;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "InlineConlikeSig{" +
                "activationMaybe=" + activationMaybe +
                ", qName=" + qName +
                '}';
    }
}
