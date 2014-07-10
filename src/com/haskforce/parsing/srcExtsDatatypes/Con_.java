package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Con_ l (QName l)
 */
public class Con_ extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "Con_{" +
                "qName=" + qName +
                '}';
    }
}
