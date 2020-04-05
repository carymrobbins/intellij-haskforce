package com.haskforce.jps.ghc

import java.io.File

import com.haskforce.macros.string.dedent
import com.haskforce.test.AssertMixin
import junit.framework.TestCase
import org.jetbrains.jps.incremental.messages.BuildMessage.Kind._

/** Tests for [[GhcMessageParser]]. */
class GhcMessageParserTest extends TestCase with AssertMixin {

  import GhcMessageParserTest._

  def testFixtures(): Unit = {
    fixtures.foreach { fixture =>
      try {
        runParser(fixture) === fixture.output
      } catch {
        case e: AssertionError =>
          throw new AssertionError(s"Fixture '${fixture.name}' failed: ${e.getMessage}", e)
      }
    }
  }
}

object GhcMessageParserTest {

  import GhcMessageParser._

  final case class Fixture(name: String, input: Seq[String], output: Seq[Result])

  /** Dummy module dir for providing the absolute path to sources. */
  private val moduleDir = "module"

  private val fixtures: Seq[Fixture] = Seq(

    Fixture(
      name = "Simple warning",
      input = Seq(
        "src/Main.hs:3:1: Warning:\n",
        "    Top-level binding with no type signature: main :: IO ()\n",
        "\n"
      ),
      output = Seq(
        warn(
          "Top-level binding with no type signature: main :: IO ()",
          "src/Main.hs", 3, 1
        )
      )
    ),

    Fixture(
      name = "Warning in context",
      input = Seq(
        "/home/user/.local/bin/etlas build\n",
        "Preprocessing executable 'etlas-ex' for etlas-ex-0.1.0.0..\n",
        "Building executable 'etlas-ex' for etlas-ex-0.1.0.0..\n",
        "[1 of 1] Compiling Main             ( src/Main.hs, dist/build/etlas-ex/etlas-ex-tmp/Main.jar )\n",
        "\n",
        "src/Main.hs:3:1: Warning:\n",
        "    Top-level binding with no type signature: main :: IO ()\n",
        "\n",
        "src/Main.hs:5:1: Warning:\n",
        "    Top-level binding with no type signature:\n",
        "      foo :: forall a. [a] -> [a]\n",
        "\n",
        "<no location info>: \n",
        "Failing due to -Werror.\n"
      ),
      output = Seq(
        info("/home/user/.local/bin/etlas build"),
        info("Preprocessing executable 'etlas-ex' for etlas-ex-0.1.0.0.."),
        info("Building executable 'etlas-ex' for etlas-ex-0.1.0.0.."),
        info("[1 of 1] Compiling Main             ( src/Main.hs, dist/build/etlas-ex/etlas-ex-tmp/Main.jar )"),
        warn("Top-level binding with no type signature: main :: IO ()", "src/Main.hs", 3, 1),
        warn(dedent("""
          Top-level binding with no type signature:
            foo :: forall a. [a] -> [a]
        """), "src/Main.hs", 5, 1),
        error("Failing due to -Werror.")
      )
    ),

    Fixture(
      name = "Werror",
      input = Seq(
        "Warning: The package list for 'hackage.haskell.org' is 35 days old.\n",
        "Run 'etlas update' to get the latest list of available packages.\n",
        "Warning: The package list for 'etlas.typelead.com' is 35 days old.\n",
        "Run 'etlas update' to get the latest list of available packages.\n",
        "Resolving dependencies...\n",
        "Configuring etlas-ex-0.1.0.0...\n",
        "Preprocessing executable 'etlas-ex' for etlas-ex-0.1.0.0..\n",
        "Building executable 'etlas-ex' for etlas-ex-0.1.0.0..\n",
        "[1 of 1] Compiling Main             ( src/Main.hs, dist/build/etlas-ex/etlas-ex-tmp/Main.jar )\n",
        "\n",
        "src/Main.hs:3:1: Warning:\n",
        "    Top-level binding with no type signature: main :: IO ()\n",
        "\n",
        "src/Main.hs:11:1: Warning:\n",
        "    Top-level binding with no type signature:\n",
        "      f :: forall a. (Eq a, Num a) => Maybe a -> [Char]\n",
        "\n",
        "<no location info>:\n",
        "Failing due to -Werror.\n"
      ),
      output = Seq(
        warn(dedent("""
          Warning: The package list for 'hackage.haskell.org' is 35 days old.
          Run 'etlas update' to get the latest list of available packages.
        """)),
        warn(dedent("""
          Warning: The package list for 'etlas.typelead.com' is 35 days old.
          Run 'etlas update' to get the latest list of available packages.
        """)),
        info("Resolving dependencies..."),
        info("Configuring etlas-ex-0.1.0.0..."),
        info("Preprocessing executable 'etlas-ex' for etlas-ex-0.1.0.0.."),
        info("Building executable 'etlas-ex' for etlas-ex-0.1.0.0.."),
        info("[1 of 1] Compiling Main             ( src/Main.hs, dist/build/etlas-ex/etlas-ex-tmp/Main.jar )"),
        // TODO: Post-process when -Werror to turn warnings into errors.
        warn(dedent("""
              Top-level binding with no type signature: main :: IO ()
        """), "src/Main.hs", 3, 1),
        warn(dedent("""
              Top-level binding with no type signature:
                f :: forall a. (Eq a, Num a) => Maybe a -> [Char]
        """), "src/Main.hs", 11, 1),
        error("Failing due to -Werror.")
      )
    )
  )

  private def info(message: String) = Result(INFO, message, None, None, None)

  private def warn(message: String, path: String, line: Long, col: Long) = Result(
    WARNING, message, Some(moduleDir + File.separator + path), Some(line), Some(col)
  )

  private def warn(message: String) = Result(WARNING, message, None, None, None)

  // TODO: Add a test for this?
  // private def error(message: String, path: String, line: Long, col: Long) = Result(
  //   ERROR, message, Some(moduleDir + File.separator + path), Some(line), Some(col)
  // )

  private def error(message: String) = Result(ERROR, message, None, None, None)

  private def runParser(fixture: Seq[String]): Seq[GhcMessageParser.Result] = {
    val p = new GhcMessageParser(moduleDir)
    fixture.flatMap(p.process) ++ p.flush()
  }

  private def runParser(fixture: Fixture): Seq[GhcMessageParser.Result] = {
    runParser(fixture.input)
  }
}
