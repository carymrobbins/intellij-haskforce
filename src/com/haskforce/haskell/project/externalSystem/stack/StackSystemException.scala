package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.ExternalSystemException

class StackSystemException private (message: String, cause: Throwable)
  extends ExternalSystemException(message, cause) {

  def this(
    message: String,
    cause: Throwable = null,
    vars: List[(String, Any)] = Nil
  ) = {
    this(
      message = StackSystemException.updatedMessage(message, cause, vars),
      cause = cause
    )
  }
}

object StackSystemException {

  private def updatedMessage(
    message: String,
    cause: Throwable,
    vars: List[(String, Any)]
  ): String = {
    val exceptionVars: Iterator[(String, Any)] = {
      Option(cause).iterator.flatMap { e =>
        List(
          "exception" -> e,
          "stackTrace" -> renderStackTrace(e)
        )
      }
    }
    val renderedVars: Iterator[String] = {
      (vars.iterator ++ exceptionVars).map {
        case (k, v) => s"$k=$v"
      }
    }
    val elems: Iterator[String] = Iterator(message) ++ renderedVars
    elems.mkString("; ")
  }

  private def renderStackTrace(exception: Throwable): String = {
    s"{\n${exception.getStackTrace.mkString("\n")}\n}"
  }
}
