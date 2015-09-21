package com.haskforce.importWizard.stack

import com.intellij.ide.util.projectWizard.{ModuleWizardStep, WizardContext}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{CharsetToolkit, VirtualFile}
import com.intellij.projectImport.ProjectImportProvider

/**
 * Provides the UI for importing a Stack project.
 */
class StackProjectImportProvider(builder: StackProjectImportBuilder)
extends ProjectImportProvider(builder) {
  override def createSteps(context: WizardContext): Array[ModuleWizardStep] = Array(
    new StackProjectImportStep(context),
    new StackSelectImportedProjectsStep(context)
  )

  /**
   * Checks to see if a given file or directory can be imported as a Stack project.
   */
  override def canImport(fileOrDirectory: VirtualFile, project: Project): Boolean = {
    // If we're not importing a directory, validate it as a file.
    if (!fileOrDirectory.isDirectory) return canImportFromFile(fileOrDirectory)
    for (stackFile <- Option(fileOrDirectory.findChild("stack.yaml"))) {
      if (canImportFromFile(stackFile)) return true
    }
    val stackFiles = fileOrDirectory.getChildren.filter(_.getName != "stack.yaml")
    stackFiles.exists(canImportFromFile)
  }

  /**
   * Confirms that the Stack config file to be imported is valid
   * and, if so, set the builder's parameters accordingly.
   */
  override def canImportFromFile(file: VirtualFile): Boolean = {
    val content = new String(file.contentsToByteArray(), CharsetToolkit.UTF8)
    // Bail out if we can't parse the yaml file.
    if (StackYaml.fromString(content).isLeft) return false
    // If we can parse it, go ahead and update the builder accordingly.
    builder.params.stackYamlPath = Some(file.getCanonicalPath)
    true
  }
}
