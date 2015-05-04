package com.haskforce.cabal.settings.ui

import javax.swing.text.JTextComponent

import com.haskforce.HaskellModuleType
import com.haskforce.Implicits._
import com.haskforce.cabal.settings.{CabalBuildType, AddCabalPackageOptions}
import com.haskforce.utils.{FileUtil, CabalExecutor, ExecUtil}
import com.intellij.openapi.module.{ModuleManager, Module}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

import scala.annotation.tailrec

/**
 * Helper utility to share functionality between the various forms which create Cabal packages.
 */
object AddCabalPackageUtil {
  def buildOptions(maybeProject: Option[Project], form: AddCabalPackageForm,
                   maybeModule: Option[Module], packageName: String, rootDir: String): AddCabalPackageOptions = {
    AddCabalPackageOptions(
      maybeModule,
      packageName,
      form.getPackageVersionField.getText,
      form.getBuildTypeField.getSelectedItem.asInstanceOf[CabalBuildType],
      rootDir,
      form.getSourceDirField.getText,
      form.getCabalVersionField.getText,
      cleanString(form.getLicenseField.getSelectedItem.toString),
      cleanString(form.getAuthorField.getText),
      cleanString(form.getEmailField.getText),
      cleanString(form.getHomepageField.getText),
      cleanString(form.getSynopsisField.getText),
      cleanString(form.getCategoryField.getSelectedItem.toString),
      form.getLanguageField.getSelectedItem.toString,
      form.getGenerateCommentsField.isSelected
    )
  }

  def setupFields(maybeProject: Option[Project], form: AddCabalPackageForm) {
    setupBuildType(form)
    setupSourceDir(form)
    setupVersion(form)
    setupCabalVersion(maybeProject, form)
    setupLicense(form)
    setupAuthor(maybeProject, form)
    setupEmail(maybeProject, form)
    setupCategory(form)
    setupLanguage(form)
  }

  val languages = Seq("Haskell2010", "Haskell98")

  def setupLanguage(form: AddCabalPackageForm) {
    languages.foreach(form.getLanguageField.addItem)
    form.getLanguageField.setSelectedIndex(0)
  }

  val categories = Seq(
    "Codec",
    "Concurrency",
    "Control",
    "Data",
    "Database",
    "Development",
    "Distribution",
    "Game",
    "Graphics",
    "Language",
    "Math",
    "Network",
    "Sound",
    "System",
    "Testing",
    "Text",
    "Web"
  )

  def setupCategory(form: AddCabalPackageForm) {
    ("" +: categories).foreach(form.getCategoryField.addItem)
  }

  def setupEmail(maybeProject: Option[Project], form: AddCabalPackageForm) {
    maybeSetTextFromCommandLine(maybeProject.nullMap(_.getBasePath), form.getEmailField, "git", "config", "user.email")
  }

  def setupAuthor(maybeProject: Option[Project], form: AddCabalPackageForm) {
    maybeSetTextFromCommandLine(maybeProject.nullMap(_.getBasePath), form.getAuthorField, "git", "config", "user.name")
  }

  val licenses = Seq(
    "GPL-2",
    "GPL-3",
    "LGPL-2.1",
    "LGPL-3",
    "AGPL-3",
    "BSD2",
    "BSD3",
    "MIT",
    "MPL-2.0",
    "Apache-2.0",
    "PublicDomain",
    "AllRightsReserved"
  )

  def setupLicense(form: AddCabalPackageForm) {
    ("" +: licenses).foreach(form.getLicenseField.addItem)
  }

  def setupCabalVersion(maybeProject: Option[Project], form: AddCabalPackageForm): Unit = {
    for(
      project <- maybeProject;
      cabal <- CabalExecutor.create(project, None).right.toOption;
      version <- cleanString(cabal.getNumericVersion)
    )
    form.getCabalVersionField.setText(">=" + version)
  }

  def maybeSetTextFromCommandLine(workDir: Option[String], component: JTextComponent,
                                  command: String, args: String*): Unit = {
    maybeSetText(component, ExecUtil.readCommandLine(workDir.orNull, command, args: _*))
  }

  def maybeSetText(textComponent: JTextComponent, value: String): Unit = {
    cleanString(value).foreach(textComponent.setText)
  }

  def cleanString(s: String): Option[String] = Option(s).map(_.trim).filter(_.nonEmpty)

  def setupVersion(form: AddCabalPackageForm) {
    form.getPackageVersionField.setText("0.1.0.0")
  }

  def setupBuildType(form: AddCabalPackageForm) {
    Seq(CabalBuildType.Executable, CabalBuildType.Library).foreach(form.getBuildTypeField.addItem)
    form.getBuildTypeField.setSelectedIndex(0)
  }

  def setupSourceDir(form: AddCabalPackageForm): Unit = form.getSourceDirField.setText("src")

  def buildArgs(options: AddCabalPackageOptions): Seq[String] = {
    Seq[Option[String]](
      Some("--non-interactive"),
      asArg("package-name", options.packageName),
      asArg("version", options.packageVersion),
      Some(asArg(options.buildType)),
      asArg("package-dir", options.rootDir),
      asArg("source-dir", options.sourceDir),
      asArg("cabal-version", options.cabalVersion),
      options.license.flatMap(asArg("license", _)),
      options.author.flatMap(asArg("author", _)),
      options.email.flatMap(asArg("email", _)),
      options.homepage.flatMap(asArg("homepage", _)),
      options.synopsis.flatMap(asArg("synopsis", _)),
      options.category.flatMap(asArg("category", _)),
      asArg("language", options.language),
      if (options.generateComments) None else Some("--no-comments")
    ).flatten
  }

  private def asArg(key: String, value: String): Option[String] = {
    Option(value).filter(_.nonEmpty).map(v => s"--$key=$value")
  }

  private def asArg(buildType: CabalBuildType): String = buildType match {
    case CabalBuildType.Executable => "--is-executable"
    case CabalBuildType.Library => "--is-library"
  }

  def importCabalPackage(project: Project)(file: VirtualFile): Unit = {
    importCabalPackage(project, file.getParent.getPath, file.getNameWithoutExtension)
  }

  /**
   * Create a Haskell module from a Cabal package.
   * TODO: Parse Cabal file and provide source directories.
   */
  def importCabalPackage(project: Project, rootDir: String, packageName: String): Unit = {
    val moduleName = determineProperModuleName(project, packageName)
    val moduleManager = ModuleManager.getInstance(project)
    val moduleBuilder = HaskellModuleType.getInstance.createModuleBuilder()
    moduleBuilder.setModuleFilePath(FileUtil.join(rootDir, moduleName + ".iml"))
    moduleBuilder.setContentEntryPath(rootDir)
    moduleBuilder.setName(moduleName)
    moduleBuilder.createModule(moduleManager.getModifiableModel)
    moduleBuilder.commit(project)
  }

  /**
   * Determines a proper module name that doesn't clash with existing modules.
   */
  private def determineProperModuleName(project: Project, name: String): String = {
    val moduleNames = ModuleManager.getInstance(project).getModules.map(_.getName.toLowerCase)
    @tailrec
    def loop(suffix: Int): String = {
      // Append "-suffix" to name if there are conflicts.
      val newName = name + (if (suffix == 0) "" else "-" + suffix)
      if (moduleNames.contains(newName)) loop(suffix + 1)
      else newName
    }
    loop(0)
  }
}
