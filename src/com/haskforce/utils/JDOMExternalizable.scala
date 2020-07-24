package com.haskforce.utils

import com.intellij.execution.configuration.{EnvironmentVariablesComponent, EnvironmentVariablesData}
import com.intellij.openapi.util.JDOMExternalizerUtil
import com.intellij.util.xmlb.Constants
import org.jdom.Element

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe.TypeTag

/** Type class for serializing configuration to the file system. */
trait JDOMExternalizable[A] {
  def readExternal(parent: Element): A
  def writeExternal(parent: Element, a: A): Unit
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
    override def readExternal(parent: Element): A = r(parent)
    override def writeExternal(parent: Element, a: A): Unit = w(parent, a)
  }

  // We could perform derivation with shapeless, but we really don't need to.
  // We can just do it by hand for now for the cases needed.
  // Use the apply/unapply methods as arguments to your desired derive method.

  //noinspection DuplicatedCode
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

  //noinspection DuplicatedCode
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

  //noinspection DuplicatedCode
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

  //noinspection DuplicatedCode
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

  //noinspection DuplicatedCode
  def derive5[CC <: Product : TypeTag, A, B, C, D, E](
    ap: (A, B, C, D, E) => CC,
    un: CC => Option[(A, B, C, D, E)]
  )(
    implicit
    aField: JDOMFieldExternalizable[A],
    bField: JDOMFieldExternalizable[B],
    cField: JDOMFieldExternalizable[C],
    dField: JDOMFieldExternalizable[D],
    eField: JDOMFieldExternalizable[E],
  ): JDOMExternalizable[CC] = {
    val List(aName, bName, cName, dName, eName) = MetaUtil.caseFieldNames[CC]
    instance(
      el => ap(
        aField.readField(aName, el),
        bField.readField(bName, el),
        cField.readField(cName, el),
        dField.readField(dName, el),
        eField.readField(eName, el)
      ),
      (el, cc) => {
        val (a, b, c, d, e) = unsafeUnapplyGet(un(cc))
        aField.writeField(aName, el, a)
        bField.writeField(bName, el, b)
        cField.writeField(cName, el, c)
        dField.writeField(dName, el, d)
        eField.writeField(eName, el, e)
      }
    )
  }

  //noinspection DuplicatedCode
  def derive6[CC <: Product : TypeTag, A, B, C, D, E, F](
    ap: (A, B, C, D, E, F) => CC,
    un: CC => Option[(A, B, C, D, E, F)]
  )(
    implicit
    aField: JDOMFieldExternalizable[A],
    bField: JDOMFieldExternalizable[B],
    cField: JDOMFieldExternalizable[C],
    dField: JDOMFieldExternalizable[D],
    eField: JDOMFieldExternalizable[E],
    fField: JDOMFieldExternalizable[F],
  ): JDOMExternalizable[CC] = {
    val List(aName, bName, cName, dName, eName, fName) = MetaUtil.caseFieldNames[CC]
    instance(
      el => ap(
        aField.readField(aName, el),
        bField.readField(bName, el),
        cField.readField(cName, el),
        dField.readField(dName, el),
        eField.readField(eName, el),
        fField.readField(fName, el),
      ),
      (el, cc) => {
        val (a, b, c, d, e, f) = unsafeUnapplyGet(un(cc))
        aField.writeField(aName, el, a)
        bField.writeField(bName, el, b)
        cField.writeField(cName, el, c)
        dField.writeField(dName, el, d)
        eField.writeField(eName, el, e)
        fField.writeField(eName, el, f)
      }
    )
  }

  private def unsafeUnapplyGet[A](o: Option[A]): A = o.getOrElse {
    throw new RuntimeException("unapply unexpectedly returned None")
  }

  implicit val string: JDOMExternalizable[String] = {
    new JDOMExternalizable[String] {
      override def readExternal(parent: Element): String = {
        val el = parent.getChild(Constants.OPTION)
        if (el == null) throw new IllegalArgumentException("option element not found")
        val res = el.getAttribute(Constants.VALUE)
        if (res == null) throw new IllegalArgumentException("value attribute not found")
        res.getValue
      }

      override def writeExternal(parent: Element, s: String): Unit = {
        val el = new Element(Constants.OPTION)
        el.setAttribute(Constants.VALUE, s)
        parent.addContent(el)
        ()
      }
    }
  }
}

/**
 * Type class for serializing configuration fields to the file system.
 * Used for deriving instances of the JDOMExternalizable type class.
 */
trait JDOMFieldExternalizable[A] {
  def readField(name: String, parent: Element): A
  def writeField(name: String, parent: Element, a: A): Unit

  final def imap[B](f: A => B, g: B => A): JDOMFieldExternalizable[B] = {
    JDOMFieldExternalizable.instance(
      (name, parent) => f(readField(name, parent)),
      (name, parent, b) => writeField(name, parent, g(b))
    )
  }
}

object JDOMFieldExternalizable {

  def apply[A](implicit ev: JDOMFieldExternalizable[A]): JDOMFieldExternalizable[A] = ev

  def instance[A](
    r: (String, Element) => A,
    w: (String, Element, A) => Unit
  ): JDOMFieldExternalizable[A] = new JDOMFieldExternalizable[A] {
    override def readField(n: String, parent: Element): A = r(n, parent)
    override def writeField(n: String, parent: Element, a: A): Unit = w(n, parent, a)
  }

  def fromJDOMExternalizable[A](
    A: JDOMExternalizable[A]
  ): JDOMFieldExternalizable[A] = {
    new JDOMFieldExternalizable[A] {
      override def readField(name: String, parent: Element): A = {
        val el = requireOption(parent, name)
        A.readExternal(el)
      }

      override def writeField(name: String, parent: Element, a: A): Unit = {
        val el = JDOMExternalizerUtil.writeOption(parent, name)
        A.writeExternal(el, a)
      }
    }
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

  // Not implicit because that seems a little dangerous.
  def iter[A](
    implicit A: JDOMExternalizable[A]
  ): JDOMFieldExternalizable[Iterator[A]] = new JDOMFieldExternalizable[Iterator[A]] {
    override def readField(name: String, parent: Element): Iterator[A] = {
      requireOption(parent, name)
        .getChildren.iterator().asScala
        .map { item =>
          if (item.getName != "item") {
            throw new IllegalStateException(
              s"Expected iterable element name 'item' but was '${item.getName}'"
            )
          }
          A.readExternal(item)
        }
    }

    override def writeField(name: String, parent: Element, xs: Iterator[A]): Unit = {
      val el = JDOMExternalizerUtil.writeOption(parent, name)
      xs.foreach { x =>
        val item = new Element("item")
        el.addContent(item)
        A.writeExternal(item, x)
      }
    }
  }

  implicit def list[A](
    implicit A: JDOMExternalizable[A]
  ): JDOMFieldExternalizable[List[A]] = {
    iter[A].imap(_.toList, _.iterator)
  }

  implicit def set[A](
    implicit A: JDOMExternalizable[A]
  ): JDOMFieldExternalizable[Set[A]] = {
    iter[A].imap(_.toSet, _.iterator)
  }

  implicit def option[A](
    implicit A: JDOMExternalizable[A]
  ): JDOMFieldExternalizable[Option[A]] = {
    iter[A].imap(_.toStream.headOption, _.iterator)
  }

  private def requireOption(parent: Element, name: String): Element = {
    JDOMExternalizerUtil.readOption(parent, name) match {
      case null => throw new IllegalArgumentException(s"Field not found: $name")
      case el => el
    }
  }
}
