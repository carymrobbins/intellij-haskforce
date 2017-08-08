package com.haskforce.utils

import com.intellij.execution.configuration.{EnvironmentVariablesComponent, EnvironmentVariablesData}
import com.intellij.openapi.util.JDOMExternalizerUtil
import org.jdom.Element
import shapeless._

/** Type class for serializing configuration to the file system. */
trait JDOMExternalizable[A] {
  def readExternal(element: Element): A
  def writeExternal(element: Element, a: A): Unit
}

object JDOMExternalizable {

  def apply[A](implicit ev: JDOMExternalizable[A]): JDOMExternalizable[A] = ev

  def instance[A](
    r: Element => A,
    w: (Element, A) => Unit
  ): JDOMExternalizable[A] = new JDOMExternalizable[A] {
    override def readExternal(e: Element): A = r(e)
    override def writeExternal(e: Element, a: A): Unit = w(e, a)
  }

  def derive[A](implicit ev: Derived[A]): JDOMExternalizable[A] = ev

  def generic[A, L <: HList](
    implicit
    g: LabelledGeneric.Aux[A, L],
    lExt: JDOMExternalizable[L]
  ): JDOMExternalizable[A] = instance(
    e => {
      g.from(lExt.readExternal(e))
    },
    (e, a) => {
      lExt.writeExternal(e, g.to(a))
    }
  )

  implicit def hlist[K <: Symbol, H, T <: HList](
    implicit
    w: Witness.Aux[K],
    hField: JDOMFieldExternalizable[H],
    tExt: JDOMExternalizable[T]
  ): JDOMExternalizable[labelled.FieldType[K, H] :: T] = instance(
    e => {
      labelled.field[K](hField.readField(w.value.name, e)) :: tExt.readExternal(e)
    },
    (e, a) => a match { case h :: t =>
      hField.writeField(w.value.name, e, h)
      tExt.writeExternal(e, t)
    }
  )

  implicit val hnil: JDOMExternalizable[HNil] = JDOMExternalizable.instance(
    _ => HNil,
    (_, _) => ()
  )

  type Derived[A] = JDOMExternalizable[A] with DerivedTag
  trait DerivedTag
  object DerivedTag {
    implicit def derived[A, L <: HList](
      implicit
      g: LabelledGeneric.Aux[A, L],
      ext: JDOMExternalizable[L]
    ): Derived[A] = generic[A, L].asInstanceOf[Derived[A]]
  }
}

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

  implicit val string: JDOMFieldExternalizable[String] = instance(
    (n, e) => JDOMExternalizerUtil.readField(e, n),
    (n, e, a) => JDOMExternalizerUtil.writeField(e, n, a)
  )

  implicit val envVarData: JDOMFieldExternalizable[EnvironmentVariablesData] = instance(
    (_, e) => EnvironmentVariablesData.readExternal(e),
    (_, e, a) => EnvironmentVariablesComponent.writeExternal(e, a.getEnvs)
  )
}
