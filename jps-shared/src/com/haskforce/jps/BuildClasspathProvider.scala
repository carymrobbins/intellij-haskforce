package com.haskforce.jps

import java.io.File
import java.util

import com.intellij.compiler.server.BuildProcessParametersProvider
import com.intellij.util.PathUtil

/** Builds classpath for compile server dynamically from the plugin's lib jars. */
final class BuildClasspathProvider extends BuildProcessParametersProvider {

  override lazy val getClassPath: util.List[String] = {
    val cp = new util.ArrayList[String]()
    BASE_CLASSPATH.foreach { cp.add }
    cp.add(getClassesDir.getAbsolutePath)
    cp
  }

  private val BASE_CLASSPATH = List(
    "jps/jps-plugin.jar",
    "jps-shared.jar"
  )

  /** Returns a path analogous to: ~/.IntelliJIdeaX/config/plugins/intellij-haskforce/lib/ */
  private def getClassesDir: File = {
    val dir = new File(PathUtil.getJarPathForClass(getClass)).getParentFile
    if (!dir.isDirectory) throw new AssertionError(s"Could not find plugin classes directory: $dir")
    dir
  }
}
