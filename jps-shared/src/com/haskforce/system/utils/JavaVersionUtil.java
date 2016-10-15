package com.haskforce.system.utils;

import org.jetbrains.annotations.NotNull;

import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Utility for obtaining the running Java version. */
public abstract class JavaVersionUtil {

  public static boolean isAtLeastJava8() {
    return getJavaVersion().compareTo(java8) >= 0;
  }

  @NotNull
  public static BigDecimal getJavaVersion() {
    if (cachedJavaVersion != null) return cachedJavaVersion;
    String v = System.getProperty("java.version");
    if (v == null) throw new RuntimeException("Property 'java.version' not defined");
    Matcher m = pattern.matcher(v);
    if (!m.matches()) throw new RuntimeException("Invalid 'java.version' value: " + v);
    cachedJavaVersion = new BigDecimal(m.group(1));
    return cachedJavaVersion;
  }

  private static BigDecimal cachedJavaVersion = null;

  private static Pattern pattern = Pattern.compile("^(\\d+(\\.\\d+)?).*");

  private static BigDecimal java8 = new BigDecimal("1.8");
}
