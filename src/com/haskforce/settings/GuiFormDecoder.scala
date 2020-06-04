package com.haskforce.settings

import javax.swing.{JCheckBox, JTextField}

trait GuiFormDecoder[I, O] {
  def decode(i: I): Either[GuiFormDecoder.Failure, O]
}

object GuiFormDecoder {

  final case class Failure(input: Any, message: String)

  private def mk[I, O](f: I => Either[String, O]): GuiFormDecoder[I, O] = {
    i => f(i).left.map(Failure(i, _))
  }

  final case class from[I](private val input: I) extends AnyVal {
    def decode[O](implicit d: GuiFormDecoder[I, O]): Either[Failure, O] = {
      d.decode(input)
    }
  }

  implicit val jCheckBox_Bool: GuiFormDecoder[JCheckBox, Boolean] = {
    i => Right(i.isSelected)
  }

  implicit def jTextField[A](
    implicit d: GuiFormDecoder[String, A]
  ): GuiFormDecoder[JTextField, A] = {
    i => d.decode(i.getText)
  }

  implicit val string_OptionLong: GuiFormDecoder[String, Option[Long]] = mk {
    s =>
      if (s.isEmpty) {
        Right(None)
      } else {
        try {
          Right(Some(s.toLong))
        } catch {
          case _: NumberFormatException =>
            Left(s"Invalid input for number: '$s'")
        }
      }
  }

  implicit val string_String: GuiFormDecoder[String, String] = mk {
    s => Right(s)
  }

  implicit val string_Long: GuiFormDecoder[String, Long] = mk {
    s =>
      try {
        Right(s.toLong)
      } catch {
        case _: NumberFormatException =>
          Left(s"Invalid input for number: '$s'")
      }
  }

  implicit def string_Option[A](
    implicit d: GuiFormDecoder[String, A]
  ): GuiFormDecoder[String, Option[A]] = {
    s => if (s.isEmpty) Right(None) else d.decode(s).map(Option(_))
  }
}
