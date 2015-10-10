package com.haskforce.macros

import java.util.regex.Pattern

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
  * Package-like object for importing String macros.
  */
object string extends StringMacros

/**
 * Provides String macros via mixin.  Useful for re-exporting macros.
 */
trait StringMacros {
  import StringMacrosImpl._

  def join(sep: String)(elems: String*): String = macro joinImpl
  def oneline(str: String): String = macro onelineImpl
  def dedent(str: String): String = macro dedentImpl
}

object StringMacrosImpl {
  /**
   * Provides the implementation for the join macro.
   */
  def joinImpl
      (c: blackbox.Context)
      (sep: c.Expr[String])
      (elems: c.Expr[String]*)
      : c.Expr[String] = {
    import c.universe._
    if (elems.length < 2) {
      c.abort(c.enclosingPosition,
        s"expected at least 2 parameters, got ${elems.length}"
      )
    }
    elems.reduce((acc, s) => c.Expr[String](q"$acc + $sep + $s"))
  }

  /**
   * Provides the implementation for the oneline macro.
   */
  def onelineImpl(c: blackbox.Context)(str: c.Expr[String]): c.Expr[String] = {
    import c.universe._
    str match {
      // String literal
      case c.Expr(Literal(Constant(s))) =>
        val result = makeOneline(show(s))
        c.Expr[String](Literal(Constant(result)))

      // String context
      case c.Expr(Apply(Select(Apply(strContext, parts), prefix), params)) =>
        val strings = parts.map { case Literal(Constant(p)) => show(p) }
        val cleaned = strings match {
          case first +: mid :+ last =>
            maybePadR(first) +: mid.map(maybePadBoth) :+ maybePadL(last)
          // Empty or singleton case.
          case _ => strings.map(makeOneline)
        }
        val newParts = cleaned.map(s => Literal(Constant(s)))
        c.Expr(Apply(Select(Apply(strContext, newParts), prefix), params))

      case _ =>
        c.abort(c.enclosingPosition, s"Expected string literal or context, got: $str")
    }
  }

  /**
   * Helper method to reuse logic between the string literal and context cases.
   */
  private def makeOneline(s: String): String = {
    MULTI_NEWLINE_REGEX.split(s.trim).map { line =>
      val m = LEADING_SPACE_REGEX.matcher(line)
      if (m.find()) {
        val indentSize = m.group(1).length
        line.substring(indentSize)
      } else {
        line
      }
    }.mkString(" ")
  }

  /**
   * Provides the implementation for the dedent macro.
   */
  def dedentImpl(c: blackbox.Context)(str: c.Expr[String]): c.Expr[String] = {
    import c.universe._
    str match {
      // String literal
      case c.Expr(Literal(Constant(lit))) =>
        dedentStringLiteral(c)(show(lit))

      // String context
      case c.Expr(Apply(Select(Apply(strContext, parts), prefix), params)) =>
        parts match {
          // If there is only one part, there are no interpolations, so treat it as a literal.
          case Literal(Constant(p)) :: Nil =>
            dedentStringLiteral(c)(show(p))
          // Otherwise, dedent the parts around the interpolations.
          case _ =>
            val strParts = parts.map { case Literal(Constant(p)) => show(p) }
            val dedentedParts = dedentStringContext(strParts)
            val newParts = dedentedParts.map(p => Literal(Constant(p)))
            c.Expr(Apply(Select(Apply(strContext, newParts), prefix), params))
        }

      case _ =>
        c.abort(c.enclosingPosition, s"Expected string literal or context, got: ${show(str)}")
    }
  }

  private def dedentStringLiteral(c: blackbox.Context)(lit: String): c.Expr[String] = {
    import c.universe._
    val stripped = stripTrailingWhitespace(stripInitialNewline(show(lit)))
    val newLit = dedentToMin(stripped, findMinIndent(stripped))
    c.Expr(Literal(Constant(newLit)))
  }

  /**
   * Helper method to dedent the parts of a string context.
   */
  private def dedentStringContext(parts: List[String]): List[String] = {
    val stripped = parts match {
      case head +: mid :+ last =>
        stripInitialNewline(head) +: mid :+ stripTrailingWhitespace(last)
      // Empty or singleton case.
      case _ => parts
    }
    val minIndent = findMinIndent(stripped.mkString(""))
    stripped.map(dedentToMin(_, minIndent))
  }

  /**
   * Helper method to dedent a string to a minimum indent.
   */
  private def dedentToMin(s: String, minIndent: Int): String = {
    NEWLINE_REGEX.split(s).map { line =>
      val m = LEADING_SPACE_REGEX.matcher(line)
      if (m.find() && line.length >= minIndent) line.substring(minIndent) else line
    }.mkString("\n")
  }

  /**
   * Helper method to find the miniumum indent.
   */
  private def findMinIndent(s: String): Int = {
    val indentSizes = NEWLINE_REGEX.split(s).flatMap { line =>
      val m = LEADING_SPACE_REGEX.matcher(line)
      if (m.find()) Some(m.group(1).length) else None
    }
    if (indentSizes.isEmpty) 0 else indentSizes.min
  }

  /**
   * Determines whether the string has leading spaces.
   */
  private def hasLeadingSpace(s: String): Boolean = LEADING_SPACE_REGEX.matcher(s).find()

  /**
   * Determines whether the string has trailing spaces.
   */
  private def hasTrailingSpace(s: String): Boolean = TRAILING_SPACE_REGEX.matcher(s).find()

  /**
   * If the string starts with a newline, strip the newline.
   */
  private def stripInitialNewline(s: String): String = {
    if (s.startsWith("\n")) s.substring(1) else s
  }

  /**
   * Removes any trailing whitespace from the string.
   */
  private def stripTrailingWhitespace(s: String): String = {
    TRAILING_SPACE_REGEX.matcher(s).replaceFirst("")
  }

  /**
   * If the string has leading whitespace, replace it with a single space.
   */
  private def maybePadL(s: String): String = {
    (if (hasLeadingSpace(s)) " " else "") + makeOneline(s)
  }

  /**
   * If the string has trailing whitespace, replace it with a single space.
   */
  private def maybePadR(s: String): String = {
    makeOneline(s) + (if (hasTrailingSpace(s)) " " else "")
  }

  /**
   * Combines maybePadL and maybePadR into one operation.
   */
  private def maybePadBoth(s: String): String = {
    val padL = if (hasLeadingSpace(s)) " " else ""
    val padR = if (hasTrailingSpace(s)) " " else ""
    padL + makeOneline(s) + padR
  }

  private val NEWLINE_REGEX = Pattern.compile("\\n")
  private val MULTI_NEWLINE_REGEX = Pattern.compile("\\n+")
  private val LEADING_SPACE_REGEX = Pattern.compile("^(\\s+)")
  private val TRAILING_SPACE_REGEX = Pattern.compile("\\s+$")
}
