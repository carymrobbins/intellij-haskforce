package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * RulePragmaDecl   l [Rule l]
 */
public class RulePragmaDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public Rule[] rules;

    @Override
    public String toString() {
        return "RulePragmaDecl{" +
                "rules=" + Arrays.toString(rules) +
                '}';
    }
}
