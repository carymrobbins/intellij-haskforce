package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PlaySafe l Bool
 */
public class PlaySafe extends SafetyTopType {
    public SrcInfoSpan srcInfoSpan;
    public boolean safeOrThreadSafe;

    @Override
    public String toString() {
        return safeOrThreadSafe ? "threadsafe" : "safe";
    }
}
