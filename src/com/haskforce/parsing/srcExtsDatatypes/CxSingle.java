package com.haskforce.parsing.srcExtsDatatypes;

/**
 * CxSingle l (Asst l)
 */
public class CxSingle extends ContextTopType {
    public SrcInfoSpan srcInfoSpan;
    public AsstTopType asst;

    @Override
    public String toString() {
        return "CxSingle{" +
                "asst=" + asst +
                '}';
    }
}
