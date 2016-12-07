package com.haskforce.cabal.settings

import java.io.{File, PrintWriter}

import com.haskforce.macros.string.dedent
import com.haskforce.utils.Logging
import com.haskforce.Implicits._

/** Utility for creating new files for cabal packages. */
object CabalPackageTemplate extends Logging {

  def createSetupFile(baseDir: String): Unit = {
    val newSetupFile = new File(baseDir, "Setup.hs")
    if (newSetupFile.exists()) {
      LOG.warn(s"File '${newSetupFile.getAbsolutePath}' already exists, skipping")
      return
    }
    val writer = new PrintWriter(newSetupFile, "UTF-8")
    writer.println(dedent("""
      import Distribution.Simple
      main = defaultMain
    """))
    writer.close()
  }

  def createCabalFile(baseDir: String, name: String, data: CabalFileData): Unit = {
    val newCabalFile = new File(baseDir, name + ".cabal")
    if (newCabalFile.exists()) {
      LOG.warn(s"File '${newCabalFile.getAbsolutePath}' already exists, skipping")
      return
    }
    val writer = new PrintWriter(newCabalFile, "UTF-8")
    writer.println(createCabalFileText(name, data))
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

    // Note that we use `.rtrim` to remove trailing spaces which may occur
    // due to empty template values.
    result.split('\n').map(_.rtrim).mkString("\n")
  }
}
