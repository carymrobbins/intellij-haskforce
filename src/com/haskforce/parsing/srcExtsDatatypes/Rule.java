package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * data Rule l = Rule l String (Maybe (Activation l)) (Maybe [RuleVar l]) (Exp l) (Exp l)
 */
public class Rule {
    public SrcInfoSpan srcInfoSpan;
    public String s;
    public ActivationTopType activationMaybe;
    public RuleVarTopType[] ruleVars;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "Rule{" +
                "e2=" + e2 +
                ", e1=" + e1 +
                ", ruleVars=" + Arrays.toString(ruleVars) +
                ", activationMaybe=" + activationMaybe +
                ", s='" + s + '\'' +
                '}';
    }
}
