package com.haskforce

import java.util

import com.intellij.compiler.server.BuildProcessParametersProvider

/**
 * The external builder in jps-plugin needs to have some jars added to its
 * classpath for some classes it uses.
 */
class HaskForceBuildProcessParametersProvider extends BuildProcessParametersProvider {
  override def getClassPath: util.List[String] = HaskForceRuntime.classPath
}
