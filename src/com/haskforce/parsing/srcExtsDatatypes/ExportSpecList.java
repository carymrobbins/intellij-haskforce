package com.haskforce.parsing.srcExtsDatatypes;

/**
 *data ExportSpecList l
 = ExportSpecList l [ExportSpec l]
 */
public class ExportSpecList {
    public SrcInfoSpan srcInfoSpan;
    public ExportSpecTopType[] exportSpecs;
}
