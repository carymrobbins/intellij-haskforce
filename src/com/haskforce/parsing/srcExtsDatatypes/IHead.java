package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * IHead l (QName l) [Type l]
 */
public class IHead extends InstHeadTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public TypeTopType[] types;

    @Override
    public String toString() {
        return "IHead{" +
                "qName=" + qName +
                ", types=" + Arrays.toString(types) +
                '}';
    }
}
