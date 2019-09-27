package com.haskforce.utils

import com.intellij.openapi.diagnostic.Logger

object SystemPropertyUtil {

  private val LOG = Logger.getInstance(getClass)

  def parseBoolWithDefault(prop: String, default: Boolean): Boolean = {
    System.getProperty(prop) match {
      case null => default
      case "true" => true
      case "false" => false
      case s =>
        LOG.warn(
          s"Expected boolean value for property '$prop'; got '$s'"
            + s"; defaulting to $default")
        default
    }
  }
}
