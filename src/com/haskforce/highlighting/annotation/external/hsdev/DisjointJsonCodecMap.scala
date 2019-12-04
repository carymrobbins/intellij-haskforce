package com.haskforce.highlighting.annotation.external.hsdev

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.haskforce.highlighting.annotation.external.hsdev

import scala.collection.immutable.ListMap
import scala.reflect.{ClassTag, classTag}

final case class DisjointJsonCodecMap[A <: AnyRef : ClassTag] private (
  toMap: ListMap[Class[_], JsonValueCodec[_]]
) {
  def toCodec: JsonValueCodec[A] = {
    new hsdev.DisjointJsonCodecMap.Codec[A](this)
  }
}

object DisjointJsonCodecMap {

  def apply[A <: AnyRef : ClassTag](
    kvs: KeyValue[_ <: A]*
  ): DisjointJsonCodecMap[A] = {
    new DisjointJsonCodecMap[A](ListMap(kvs.map(_.get): _*))
  }

  // Ensures that the class and the codec types match.
  final case class KeyValue[A <: AnyRef](
    get: (Class[A], JsonValueCodec[A])
  ) extends AnyVal

  implicit def toKeyValue[A <: AnyRef : ClassTag](
    codec: JsonValueCodec[A]
  ): KeyValue[A] = new KeyValue[A]((
    classTag[A].runtimeClass.asInstanceOf[Class[A]],
    codec
  ))

  class Codec[A <: AnyRef : ClassTag](
    codecMap: DisjointJsonCodecMap[A]
  ) extends JsonValueCodec[A] {

    override def decodeValue(in: JsonReader, default: A): A = {
      codecMap.toMap.foreach { case (cls, codec) =>
        in.setMark()
        codec.asInstanceOf[JsonValueCodec[A]].decodeValue(
          in, null.asInstanceOf[A]
        ) match {
          case x if x != null => return x.asInstanceOf[A]
          case _ => // noop
        }
        in.rollbackToMark()
      }
      in.decodeError(
        s"All codecs failed to decode ${classTag[A].runtimeClass}; " +
          s"codecs were: $codecMap"
      )
    }

    override def encodeValue(x: A, out: JsonWriter): Unit = {
      codecMap.toMap.get(x.getClass) match {
        case Some(codec) =>
          codec.asInstanceOf[JsonValueCodec[A]].encodeValue(x, out)
        case None =>
          throw new IllegalArgumentException(
            s"Class ${x.getClass} does not exist in codecs: $codecMap"
          )
      }
    }

    override def nullValue: A = null.asInstanceOf[A]
  }
}
