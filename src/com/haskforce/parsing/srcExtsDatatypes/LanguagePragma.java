package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *  LanguagePragma   l [Name l]
 */
public class LanguagePragma extends ModulePragmaTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType[] names;

    @Override
    public String toString() {
        return "LanguagePragma{" +
                "names=" + Arrays.toString(names) +
                '}';
    }
}
