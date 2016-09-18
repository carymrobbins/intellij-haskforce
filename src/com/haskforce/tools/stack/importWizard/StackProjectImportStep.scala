package com.haskforce.tools.stack.importWizard

import java.awt.GridBagLayout
import java.io.File
import java.util
import javax.swing._

import com.haskforce.haskell.HaskellModuleType
import com.haskforce.tools.cabal.completion.CabalFileFinder
import com.haskforce.importWizard.stack.{StackYaml, StackYamlUtil}
import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.ui.GC
import com.haskforce.utils.GuiUtil
import com.intellij.ide.util.projectWizard.WizardContext
import com.intellij.openapi.project.ProjectManager
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.projectImport.ProjectImportWizardStep

import scala.collection.JavaConversions._

/**
 * Initial UI for user to set the stack executable and stack.yaml via the import wizard.
 */
class StackProjectImportStep(context: WizardContext)
extends ProjectImportWizardStep(context) {
  // If we are in a project, use its build settings; otherwise, use the default project's.
  private lazy val defaultBuildSettings = HaskellBuildSettings.getInstance(
    Option(context.getProject).getOrElse(ProjectManager.getInstance.getDefaultProject)
  )
  private val contentPane = new JPanel(new GridBagLayout)
  private val stackPathField = new TextFieldWithBrowseButton()
  private val stackPathInfoLabel = new JLabel()
  private val stackYamlField = new TextFieldWithBrowseButton()
  private val stackYamlInfoLabel = new JLabel()
  private lazy val builder = getBuilder.asInstanceOf[StackProjectImportBuilder]
  private lazy val parameters = builder.params

  setupComponents()

  private def setupComponents(): Unit = {
    val gcBase = GC.pad(10, 5).northWest
    val gcLabel = gcBase.weight(0.25)
    val gcField = gcBase.weight(0.75)

    var y = 0
    GuiUtil.addFolderListener(stackPathField, "stack executable")
    contentPane.add(new JLabel("Stack executable:"), gcLabel.grid(0, y))
    contentPane.add(stackPathField, gcField.grid(1, y).fillHorizontal)
    contentPane.add(stackPathInfoLabel, gcField.grid(1, y + 1).fillHorizontal)

    y += 2
    GuiUtil.addFolderListener(stackYamlField, "stack yaml")
    contentPane.add(new JLabel("Stack YAML:"), gcLabel.grid(0, y))
    contentPane.add(stackYamlField, gcField.grid(1, y).fillHorizontal)
    contentPane.add(stackYamlInfoLabel, gcField.grid(1, y + 1).fillHorizontal)
  }

  override def getComponent: JComponent = contentPane

  override def updateDataModel(): Unit = {
    parameters.stackPath = Some(stackPathField.getText)
    parameters.stackYamlPath = Some(stackYamlField.getText)
    parameters.packages = filterImportablePackages()
  }

  private def filterImportablePackages(): util.List[StackYaml.Package] = {
    val packages = StackYaml.unsafeFromFile(stackYamlField.getText).packages
    val project = context.getProject
    // If this is a project import, allow importing all packages.
    if (project == null) return packages
    // Otherwise, filter this import for packages not already imported.
    val root = builder.getImportRoot
    val cabalFilePaths =
      HaskellModuleType.findModules(project)
        .flatMap(CabalFileFinder.virtualForModule).map(_.getCanonicalPath)
    packages.filter { pkg =>
      val path = StackYamlUtil.unsafeFindCabalFile(root, pkg).getCanonicalPath
      !cabalFilePaths.contains(path)
    }
  }

  private def setError(label: JLabel, err: String) = {
    label.setText(s"<html><p style='color: red'>$err</p></html>")
  }

  override def validate(): Boolean = {
    validateStackExe() && validateStackYaml()
  }

  private def validateStackExe(): Boolean = {
    val stackExe = new File(stackPathField.getText)
    if (!stackExe.canExecute) {
      setError(stackPathInfoLabel, s"'${stackExe.getName}' is not a valid executable.")
      false
    } else {
      stackPathInfoLabel.setText("")
      true
    }
  }

  private def validateStackYaml(): Boolean = {
    val projectRoot = new File(builder.getImportRoot)
    val yamlFile = new File(stackYamlField.getText)
    if (!yamlFile.exists()) {
      setError(stackYamlInfoLabel, s"'${yamlFile.getName}' does not exist.")
      false
    } else if (yamlFile.getParent != projectRoot.getPath) {
      setError(stackYamlInfoLabel, s"'${yamlFile.getName}' is not in the project root '$projectRoot'.")
      false
    } else {
      StackYaml.fromFile(yamlFile.getPath).fold(
        e => {
          setError(stackYamlInfoLabel, s"'${yamlFile.getName}' is not a valid stack yaml file. ($e)")
          false
        },
        stackYaml => {
          stackYaml.packages.foreach { pkg =>
            StackYamlUtil.findCabalFile(projectRoot.getPath, pkg).orElse {
              setError(stackYamlInfoLabel, s"Could not find cabal file in package '${pkg.path}'")
              return false
            }
          }
          true
        }
      )
    }
  }

  override def updateStep(): Unit = {
    stackPathField.setText(parameters.stackPath.getOrElse(defaultBuildSettings.guessStackPath()))
    stackYamlField.setText(parameters.stackYamlPath.getOrElse(builder.getFileToImport))
  }
}
