package com.haskforce.system.utils;

import com.intellij.openapi.diagnostic.Logger;

public class SystemUtil {
    public static final String LINE_SEPARATOR = getSystemProperty("line.separator");
    public static final String PATH_SEPARATOR = getSystemProperty("path.separator");

    private final static Logger LOG = Logger.getInstance(SystemUtil.class);

    private static String getSystemProperty(String property) {
        try {
            return System.getProperty(property);
        } catch (SecurityException var2) {
            LOG.error("Caught a SecurityException reading the system property \'" + property + "\'; the property value will default to null.");
            return null;
        }
    }
}
