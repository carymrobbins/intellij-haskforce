package com.haskforce.haskell.project.externalSystem.stack

import java.io.File
import java.nio.file.{Files, Paths}
import java.util

import com.haskforce.importWizard.stack.StackYaml
import com.haskforce.settings.HaskellBuildSettings
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.externalSystem.ExternalSystemAutoImportAware
import com.intellij.openapi.project.Project

/**
  * Handle auto-import logic for stack projects.
  * Mostly adapted from GradleAutoImportAware
  */
object StackAutoImportAware extends ExternalSystemAutoImportAware {

  private val LOG = Logger.getInstance(getClass)

  override def getAffectedExternalProjectPath(
    changedFileOrDirPath: String,
    project: Project
  ): String = {
    val file = new File(changedFileOrDirPath)
    if (file.isDirectory) return null

    // TODO: This is a hack
    if (file.getName == "stack.yaml") return changedFileOrDirPath

    // Get the project stack.yaml
    val stackFile = HaskellBuildSettings.getInstance(project).getStackFile
    // If we don't have a stack.yaml configured, don't auto-import.
    if (stackFile == null) return null

    // Check if the changed file is our configured stack.yaml.
    if (file.getCanonicalPath == new File(stackFile).getCanonicalPath) return stackFile
    // Assume a modified package.yaml/.cabal file requires auto-import.
    // TODO: We could check to ensure it's configured in the stack.yaml, but
    // this function is called often so we need to come up with some sort of
    // caching for it.
    if (file.getName == "package.yaml") return stackFile
    if (file.getName.endsWith(".cabal")) return stackFile
    null
  }

  override def getAffectedExternalProjectFiles(
    projectPath: String,
    project: Project
  ): util.List[File] = {
    val stackYamlPath =
      Option(HaskellBuildSettings.getInstance(project).getStackFile)
        .map(Paths.get(_))
        .filter(Files.exists(_))
        .getOrElse(Paths.get(projectPath, "stack.yaml"))
    // If we couldn't find a stack.yaml, give up.
    if (!Files.exists(stackYamlPath)) return util.Collections.emptyList()
    // If we couldn't parse the stack.yaml, give up.
    val stackYaml = StackYaml.fromFile(stackYamlPath.toString).valueOr { err =>
      LOG.info(s"Failed to parse $stackYamlPath: $err")
      return util.Collections.emptyList()
    }

    // Holds onto any files that should be considered external project config files.
    // This will be the stack.yaml file as well as package.yaml or .cabal files.
    val files = new util.ArrayList[File]()
    files.add(stackYamlPath.toFile)
    val stackYamlDirPath = stackYamlPath.getParent
    // Check the packages declared in the stack.yaml for a
    // package.yaml or .cabal file.
    stackYaml.packages.forEach { pkg =>
      val pkgPath = stackYamlDirPath.resolve(pkg.path)
      val packageYamlFile = pkgPath.resolve("package.yaml").toFile
      // If the package.yaml exists, we want to consider it a config file.
      // Prefer a package.yaml to a .cabal file for a given package.
      if (packageYamlFile.exists()) {
        files.add(packageYamlFile)
      } else {
        // Only check the cabal file if the package.yaml does not exist.
        // This is important to do since re-importing a project will rewrite
        // the cabal file. If we didn't do this check, the rewrite would
        // trigger us to suggest to re-import again, which we don't want.
        pkgPath.toFile.listFiles(
          (_, name) => name.endsWith(".cabal")
        ).foreach { cabalFile =>
          files.add(cabalFile)
        }
      }
      ()
    }
    files
  }
}
