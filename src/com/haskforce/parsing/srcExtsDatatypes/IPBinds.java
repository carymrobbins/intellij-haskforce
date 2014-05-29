package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *  IPBinds l [IPBind l]
 */
public class IPBinds extends BindsTopType {
    public SrcInfoSpan srcInfoSpan;
    public IPBind[] ipBinds;

    @Override
    public String toString() {
        return "IPBinds{" +
                "ipBinds=" + Arrays.toString(ipBinds) +
                '}';
    }
}
