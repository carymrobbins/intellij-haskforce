package com.haskforce.highlighting.annotation.external

import com.haskforce.test.AssertMixin
import com.haskforce.utils.ExecUtil.ExecError
import com.intellij.testFramework.UsefulTestCase
import scala.util.control.NonFatal

class HLintUtilTest extends UsefulTestCase with AssertMixin {

  def testParseVersion(): Unit = {
    type Result = Either[ExecError, VersionTriple]
    val fixtures = Vector[(String, Result => Unit)](
      "2.0.12.17"   -> (_ === Right(VersionTriple(2, 0, 12))),
      "2.0.15"      -> (_ === Right(VersionTriple(2, 0, 15))),
      "2.1"         -> (_ === Right(VersionTriple(2, 1, 0))),
      "2"           -> (_ === Right(VersionTriple(2, 0, 0))),
      "a"           -> (_ assertIsLeft()),
      ""            -> (_ assertIsLeft()),
      "."           -> (_ assertIsLeft())
    )
    val adjustments = Vector[String => String](
      identity[String],
      _ + "\n",
      "  " + _ + "  "
    )
    for {
      (baseVersionStr, assertion) <- fixtures
      adj <- adjustments
      vStr = adj(baseVersionStr)
    } yield {
      try {
        assertion(HLintUtil.parseVersion(vStr))
      } catch {
        case NonFatal(e) =>
          throw new AssertionError(s"Assertion failed for input: ${stringAsLit(vStr)}", e)
      }
    }
    ()
  }

  private def stringAsLit(s: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(s)).toString
  }
}
