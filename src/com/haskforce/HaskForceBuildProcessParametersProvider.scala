package com.haskforce

import java.util

import com.intellij.compiler.server.BuildProcessParametersProvider
import com.intellij.openapi.application.PathManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.util.containers.ContainerUtil

import scala.reflect.{ClassTag, classTag}

/**
 * The external builder in jps-plugin needs to have some jars added to its
 * classpath for some classes it uses. We figure out the right jar name
 * by picking a class from that jar and use `PathManager` to resolve the
 * jar for that class. Otherwise, keeping the version numbers updated for
 * the jars becomes cumbersome and error-prone.
 */
class HaskForceBuildProcessParametersProvider extends BuildProcessParametersProvider {

  override def getClassPath: util.List[String] = {
    mkList(
      jarPath[com.intellij.execution.ExecutionException]("platform-ide-util-io"),
      jarPath[scala.Symbol]("scala-library"),
      jarPath[scala.reflect.runtime.JavaUniverse]("scala-reflect"),
      jarPath[scalaz.Scalaz.type]("scalaz-core"),
      jarPath[org.yaml.snakeyaml.Yaml]("snakeyaml"),
      jarPath[io.estatico.newtype.NewType]("newtype"),
      jarPath[org.jetbrains.yaml.YAMLLanguage]("yaml")
    )
  }

  /**
   * Takes an array of jar names, logs each one, and builds a list to be
   * added to the external builder classpath.
   */
  private def mkList(xs: String*): util.List[String] = {
    val res = ContainerUtil.immutableList(xs: _*)
    res.forEach(x => HaskForceBuildProcessParametersProvider.LOG.info(s"Adding jar to builder classpath: $x"))
    res
  }

  /**
   * Resolve a jar from the class supplied `A` type parameter. The `name`
   * argument is only supplied for debugging purposes.
   */
  private def jarPath[A : ClassTag](name: String): String = {
    val cls = classTag[A].runtimeClass
    PathManager.getJarPathForClass(cls) match {
      case null => throw new RuntimeException(s"Failed to find jar path '$name' for class '$cls'")
      case s => s
    }
  }
}

object HaskForceBuildProcessParametersProvider {
  private val LOG = Logger.getInstance(getClass)
}
