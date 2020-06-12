package com.haskforce.utils

import com.intellij.execution.configuration.{EnvironmentVariablesComponent, EnvironmentVariablesData}
import com.intellij.openapi.util.JDOMExternalizerUtil
import org.jdom.Element

import scala.reflect.runtime.universe.TypeTag

/** Type class for serializing configuration to the file system. */
trait JDOMExternalizable[A] {
  def readExternal(element: Element): A
  def writeExternal(element: Element, a: A): Unit
}

object JDOMExternalizable {

  def readExternal[A](element: Element)(implicit ev: JDOMExternalizable[A]): A = {
    ev.readExternal(element)
  }

  def writeExternal[A](element: Element, a: A)(implicit ev: JDOMExternalizable[A]): Unit = {
    ev.writeExternal(element, a)
  }

  def apply[A](implicit ev: JDOMExternalizable[A]): JDOMExternalizable[A] = ev

  def instance[A](
    r: Element => A,
    w: (Element, A) => Unit
  ): JDOMExternalizable[A] = new JDOMExternalizable[A] {
    override def readExternal(e: Element): A = r(e)
    override def writeExternal(e: Element, a: A): Unit = w(e, a)
  }

  // We could perform derivation with shapeless, but we really don't need to.
  // We can just do it by hand for now for the cases needed.
  // Use the apply/unapply methods as arguments to your desired derive method.

  def derive1[CC <: Product : TypeTag, A](
    ap: A => CC, un: CC => Option[A]
  )(
    implicit
    aField: JDOMFieldExternalizable[A]
  ): JDOMExternalizable[CC] = {
    val List(aName) = MetaUtil.caseFieldNames[CC]
    instance(
      e => ap(aField.readField(aName, e)),
      (e, cc) => {
        val v1 = unsafeUnapplyGet(un(cc))
        aField.writeField(aName, e, v1)
      }
    )
  }

  def derive2[CC <: Product : TypeTag, A, B](
    ap: (A, B) => CC,
    un: CC => Option[(A, B)]
  )(
    implicit
    aField: JDOMFieldExternalizable[A],
    bField: JDOMFieldExternalizable[B]
  ): JDOMExternalizable[CC] = {
    val List(aName, bName) = MetaUtil.caseFieldNames[CC]
    instance(
      e => ap(
        aField.readField(aName, e),
        bField.readField(bName, e)
      ),
      (e, cc) => {
        val (a, b) = unsafeUnapplyGet(un(cc))
        aField.writeField(aName, e, a)
        bField.writeField(bName, e, b)
      }
    )
  }

  def derive3[CC <: Product : TypeTag, A, B, C](
    ap: (A, B, C) => CC,
    un: CC => Option[(A, B, C)]
  )(
    implicit
    aField: JDOMFieldExternalizable[A],
    bField: JDOMFieldExternalizable[B],
    cField: JDOMFieldExternalizable[C]
  ): JDOMExternalizable[CC] = {
    val List(aName, bName, cName) = MetaUtil.caseFieldNames[CC]
    instance(
      e => ap(
        aField.readField(aName, e),
        bField.readField(bName, e),
        cField.readField(cName, e)
      ),
      (e, cc) => {
        val (a, b, c) = unsafeUnapplyGet(un(cc))
        aField.writeField(aName, e, a)
        bField.writeField(bName, e, b)
        cField.writeField(cName, e, c)
      }
    )
  }

  def derive4[CC <: Product : TypeTag, A, B, C, D](
    ap: (A, B, C, D) => CC,
    un: CC => Option[(A, B, C, D)]
  )(
    implicit
    aField: JDOMFieldExternalizable[A],
    bField: JDOMFieldExternalizable[B],
    cField: JDOMFieldExternalizable[C],
    dField: JDOMFieldExternalizable[D]
  ): JDOMExternalizable[CC] = {
    val List(aName, bName, cName, dName) = MetaUtil.caseFieldNames[CC]
    instance(
      e => ap(
        aField.readField(aName, e),
        bField.readField(bName, e),
        cField.readField(cName, e),
        dField.readField(dName, e)
      ),
      (e, cc) => {
        val (a, b, c, d) = unsafeUnapplyGet(un(cc))
        aField.writeField(aName, e, a)
        bField.writeField(bName, e, b)
        cField.writeField(cName, e, c)
        dField.writeField(dName, e, d)
      }
    )
  }

  private def unsafeUnapplyGet[A](o: Option[A]): A = o.getOrElse {
    throw new RuntimeException("unapply unexpectedly returned None")
  }
}

/**
 * Type class for serializing configuration fields to the file system.
 * Used for deriving instances of the JDOMExternalizable type class.
 */
trait JDOMFieldExternalizable[A] {
  def readField(name: String, element: Element): A
  def writeField(name: String, element: Element, a: A): Unit
}

object JDOMFieldExternalizable {

  def apply[A](implicit ev: JDOMFieldExternalizable[A]): JDOMFieldExternalizable[A] = ev

  def instance[A](
    r: (String, Element) => A,
    w: (String, Element, A) => Unit
  ): JDOMFieldExternalizable[A] = new JDOMFieldExternalizable[A] {
    override def readField(n: String, e: Element): A = r(n, e)
    override def writeField(n: String, e: Element, a: A): Unit = w(n, e, a)
  }

  private def readFieldParser[A](p: String => A): (String, Element) => A = {
    (name, elem) => p(JDOMExternalizerUtil.readField(elem, name))
  }

  private def writeFieldFromToString[A](name: String, elem: Element, a: A): Unit = {
    JDOMExternalizerUtil.writeField(elem, name, a.toString)
  }

  implicit val string: JDOMFieldExternalizable[String] = instance(
    readFieldParser(identity),
    writeFieldFromToString
  )

  implicit val bool: JDOMFieldExternalizable[Boolean] = instance(
    readFieldParser {
      case "true" => true
      case "false" => false
      case s => throw new IllegalArgumentException(s"JDOMFieldExternalizable expected bool but got string: $s")
    },
    writeFieldFromToString
  )

  implicit val long: JDOMFieldExternalizable[Long] = instance(
    readFieldParser {
      s =>
        try {
          s.toLong
        } catch {
          case _: NumberFormatException =>
            throw new IllegalArgumentException(s"JDOMFieldExternalizable expected long but got string: $s")
        }
    },
    writeFieldFromToString
  )

  implicit val envVarData: JDOMFieldExternalizable[EnvironmentVariablesData] = instance(
    (_, e) => EnvironmentVariablesData.readExternal(e),
    (_, e, a) => EnvironmentVariablesComponent.writeExternal(e, a.getEnvs)
  )
}
