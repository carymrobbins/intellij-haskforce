package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ClsTyFam   l                     (DeclHead l) (Maybe (Kind l))
 */
public class ClsTyFam extends ClassDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DeclHeadTopType declHead;
    public KindTopType kindMaybe;

    @Override
    public String toString() {
        return "ClsTyFam{" +
                "declHead=" + declHead +
                ", kindMaybe=" + kindMaybe +
                '}';
    }
}
