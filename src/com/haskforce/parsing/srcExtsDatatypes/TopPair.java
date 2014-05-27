package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Top structure returned from parser-helper.
 */
public class TopPair {
    public ModuleTopType moduleType;
    public Comment[] comments;
    // Either the two previous values, or this.
    public String error;

    @Override
    public String toString() {
        return "TopPair{" +
                "moduleType=" + moduleType +
                ", comments=" + Arrays.toString(comments) +
                ", error='" + error + '\'' +
                '}';
    }
}
