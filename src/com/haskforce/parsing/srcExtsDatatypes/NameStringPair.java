package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * ([Name l], String)
 */
public class NameStringPair {
    public NameTopType[] names;
    public String s;

    @Override
    public String toString() {
        return "NameStringPair{" +
                "names=" + Arrays.toString(names) +
                ", s='" + s + '\'' +
                '}';
    }
}
