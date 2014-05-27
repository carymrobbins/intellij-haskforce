package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data Alt l = Alt l (Pat l) (GuardedAlts l) (Maybe (Binds l))
 */
public class Alt {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;
    public GuardedAltsTopType guardedAlts;
    public BindsTopType bindsMaybe;

    @Override
    public String toString() {
        return "Alt{" +
                "pat=" + pat +
                ", guardedAlts=" + guardedAlts +
                ", bindsMaybe=" + bindsMaybe +
                '}';
    }
}
