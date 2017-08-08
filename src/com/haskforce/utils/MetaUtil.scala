package com.haskforce.utils

import scala.reflect.runtime.universe._

/** Utilities for Scala metaprogramming. */
object MetaUtil {

  /** Access the fields of a case class A. */
  def caseFields[A <: Product](implicit tt: TypeTag[A]): List[MethodSymbol] = {
    tt.tpe.members.sorted.collect {
      case m: MethodSymbol if m.isCaseAccessor => m
    }
  }

  def caseFieldNames[A <: Product](implicit tt: TypeTag[A]): List[String]
    = caseFields[A].map(_.name.decodedName.toString)
}
