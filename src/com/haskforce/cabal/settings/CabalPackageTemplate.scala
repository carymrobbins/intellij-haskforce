package com.haskforce.cabal.settings

import java.io.{File, PrintWriter}

import com.haskforce.macros.string.dedent
import com.haskforce.utils.Logging

/** Utility for creating new files for cabal packages. */
object CabalPackageTemplate extends Logging {

  def createSetupFile(baseDir: String): Unit = {
    createFileIfNotExists(baseDir, "Setup.hs", dedent("""
      import Distribution.Simple
      main = defaultMain
    """))
  }

  def createCabalFile(baseDir: String, name: String, data: CabalFileData): Unit = {
    createFileIfNotExists(baseDir, name + ".cabal", createCabalFileText(name, data))
  }

  private def createFileIfNotExists(baseDir: String, name: String, content: => String): Unit = {
    val newFile = new File(baseDir, name)
    if (newFile.exists()) {
      LOG.warn(s"File '${newFile.getAbsolutePath}' already exists, skipping'")
      return
    }
    val writer = new PrintWriter(newFile, "UTF-8")
    writer.println(content)
    writer.close()
  }

  def createCabalFileText(name: String, data: CabalFileData): String = {
    val baseText = dedent(s"""
      name:                 $name
      version:              ${data.packageVersion}
      synopsis:             ${data.synopsis}
      -- description:
      -- license:
      -- license-file:
      homepage:             ${data.homepage}
      author:               ${data.author}
      maintainer:           ${data.maintainer}
      category:             ${data.category}
      -- copyright:
      build-type:           Simple
      -- extra-source-files:
      cabal-version:        ${data.cabalVersion}
    """)

    val componentHeader = data.componentType match {
      case CabalComponentType.Library =>
        dedent("""
          library
            -- exposed-modules:
        """)
      case CabalComponentType.Executable =>
        dedent(s"""
          executable $name
            main-is:              Main.hs
        """)
    }
    val componentText = dedent(s"""
      $componentHeader
        -- other-modules:
        -- other-extensions:
        build-depends:        base >= 4.7 && < 5
        hs-source-dirs:       ${data.sourceDir}
        default-language:     ${data.language}
    """)

    val result = dedent(s"""
      $baseText

      $componentText
    """)

    // Note that we use `rtrim` to remove trailing spaces which may occur
    // due to empty template values.
    result.split('\n').map(rtrim).mkString("\n")
  }

  // This is clever. All characters <= ' ' are whitespace or control characters.
  private def rtrim(s: String): String = {
    s.reverse.dropWhile(_ <= ' ').reverse
  }
}
