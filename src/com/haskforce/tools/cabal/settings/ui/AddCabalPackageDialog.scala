package com.haskforce.tools.cabal.settings.ui

import java.awt.Dimension
import java.awt.event.{ItemEvent, KeyEvent}
import java.io.File
import java.util.EventObject

import com.haskforce.haskell.HaskellModuleType
import com.haskforce.Implicits._
import com.haskforce.tools.cabal.settings.AddCabalPackageOptions
import com.haskforce.system.ui.ComboModuleProxy
import com.haskforce.system.utils.FileUtil
import com.intellij.openapi.project.Project
import scala.collection.JavaConversions._


import com.haskforce.tools.cabal.completion.CabalFileFinder//TODO refactor!

class AddCabalPackageDialog(
  project: Project,
  callback: (Project => AddCabalPackageOptions) => Unit
) extends AddCabalPackageDialogBase() with AddCabalPackageForm {

  private val modules = HaskellModuleType.findModules(project)
  private val nonCabalizedModules = modules.filter(m => CabalFileFinder.virtualForModule(m).isEmpty)

  setupUI()
  setupPackageNameAndRootDir()
  AddCabalPackageUtil.setupFields(Some(project), this)

  def setupUI(): Unit = {
    setMinimumSize(new Dimension(500, 600))
    // Center the dialog on screen.
    setLocationRelativeTo(null)
  }

  def setupPackageNameAndRootDir(): Unit = {
    // Allow the user to create a new cabal package (IntelliJ module).
    packageName.setEditable(true)
    // Provide a drop-down with current module names which do not have cabal files.
    // If the current project name matches the module, select it as the default value.
    nonCabalizedModules.foreach { m =>
      val item = ComboModuleProxy(m)
      packageName.addItem(item)
      if (m.getName == project.getName) {
        packageName.setSelectedItem(item)
        // Also set the default for rootDir.
        rootDir.setText(relativeToProjectRoot(m.getModuleFile.getParent.getPath))
      }
    }

    // Provide a hook so changing the packageName will also set an appropriate default rootDir.
    packageName.getEditor.getEditorComponent.addKeyListener { (e: KeyEvent) => onPackageNameChange(e) }
    packageName.addItemListener { (e: ItemEvent) => onPackageNameChange(e) }
  }

  /**
   * Update the rootDir field accordingly when the packageName field changes.
   */
  def onPackageNameChange(e: EventObject): Unit = {
    rootDir.setText(packageName.getSelectedItem match {
      case ComboModuleProxy(m) => relativeToProjectRoot(m.getModuleFile.getParent.getPath)
      case o =>
        val text = Option(packageName.getEditor.getItem).getOrElse("")
        s".${File.separator}$text"
    })
  }

  private def relativeToProjectRoot(path: String): String = FileUtil.toRelativePath(project.getBasePath, path)

  override def onOK(): Unit = {
    callback { project: Project =>
      val (selectedModule, selectedPackage) = packageName.getSelectedItem match {
        case ComboModuleProxy(m) => (Some(m), m.getName)
        case o => (None, o.toString)
      }
      val rootDirText = Option(project.getBasePath).map(FileUtil.join(_, rootDir.getText)).getOrElse(rootDir.getText)
      AddCabalPackageUtil.buildOptions(Some(project), this, selectedModule, selectedPackage, rootDirText)
    }
    super.onOK()
  }

  override def getCabalVersionField = cabalVersion
  override def getSynopsisField = synopsis
  override def getGenerateCommentsField = generateComments
  override def getComponentTypeField = buildType
  override def getCategoryField = category
  override def getSourceDirField = sourceDir
  override def getHomepageField = homepage
  override def getAuthorField = author
  override def getEmailField = email
  override def getLicenseField = license
  override def getPackageVersionField = version
  override def getLanguageField = language
}

