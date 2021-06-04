package com.haskforce

import java.util

import com.intellij.openapi.application.PathManager
import com.intellij.util.containers.ContainerUtil

import scala.reflect.{ClassTag, classTag}

object HaskForceRuntime {

  /**
   * Get the jars that HaskForce uses so we can run HaskForce code
   * in external JVM processes (external builder, external system, etc.).
   *
   * We figure out the right jar names required by HaskForce
   * by picking a class from that jar and use `PathManager` to resolve the
   * jar for that class. Otherwise, keeping the version numbers updated for
   * the jars becomes cumbersome and error-prone.
   */
  lazy val classPath: util.List[String] = ContainerUtil.immutableList(
    jarPath[com.intellij.execution.ExecutionException]("platform-ide-util-io"),
    jarPath[scala.Symbol]("scala-library"),
    jarPath[scala.reflect.runtime.JavaUniverse]("scala-reflect"),
    jarPath[scalaz.Scalaz.type]("scalaz-core"),
    jarPath[org.yaml.snakeyaml.Yaml]("snakeyaml"),
    jarPath[io.estatico.newtype.NewType]("newtype"),
    jarPath[org.jetbrains.yaml.YAMLLanguage]("yaml")
  )

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
