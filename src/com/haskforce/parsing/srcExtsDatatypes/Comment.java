package com.haskforce.parsing.srcExtsDatatypes;

/**
 *
 * data Comment = Comment Bool SrcSpan String
 */
public class Comment {
    public boolean multiline;
    public SrcSpan loc;
    public String text;
}
