package com.haskforce.tools.cabal.settings.ui

import javax.swing.JTextField
import javax.swing.text.JTextComponent

import scala.annotation.tailrec
import scalaz.syntax.id._
import com.intellij.notification.NotificationType
import com.haskforce.haskell.HaskellModuleType
import com.haskforce.Implicits._
import com.haskforce.tools.cabal.settings.{AddCabalPackageOptions, CabalComponentType}
import com.haskforce.tools.cabal.CabalExecutor
import com.haskforce.ui.SComboBox
import com.haskforce.utils.{ExecUtil, FileUtil, NotificationUtil}
import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

/**
 * Helper utility to share functionality between the various forms which create Cabal packages.
 * TODO: The `cabal init` stuff can probably go away soon, so look into removing most of this.
 */
object AddCabalPackageUtil {
  def buildOptions
      (maybeProject: Option[Project],
       form: AddCabalPackageForm,
       maybeModule: Option[Module],
       packageName: String,
       rootDir: String)
      : AddCabalPackageOptions = {
    AddCabalPackageOptions(
      maybeModule,
      packageName,
      form.getPackageVersionField.getText,
      form.getComponentTypeField.getSelectedItem.asInstanceOf[CabalComponentType],
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

  def setupFields(maybeProject: Option[Project], form: AddCabalPackageForm): Unit = {
    setupComponentType(form)
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

  def newLanguageField(): SComboBox[String] = {
    new SComboBox[String] <| setupLanguage
  }

  def setupLanguage(field: SComboBox[String]): Unit = {
    languages.foreach(field.addItem)
    field.setSelectedIndex(0)
  }

  def setupLanguage(form: AddCabalPackageForm): Unit = {
    setupLanguage(form.getLanguageField)
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

  def newCategoryField(): SComboBox[String] = {
    new SComboBox[String] <| setupCategory
  }

  def setupCategory(field: SComboBox[String]): Unit = {
    ("" +: categories).foreach(field.addItem)
  }

  def setupCategory(form: AddCabalPackageForm): Unit = {
    setupCategory(form.getCategoryField)
  }

  def newEmailField(maybeProject: Option[Project] = None): JTextField = {
    new JTextField() <| (f => setupEmail(maybeProject, f))
  }

  def setupEmail(maybeProject: Option[Project], field: JTextField): Unit = {
    maybeSetTextFromCommandLine(
      maybeProject.nullMap(_.getBasePath), field,
      "git", "config", "user.email"
    )
  }

  def setupEmail(maybeProject: Option[Project], form: AddCabalPackageForm): Unit = {
    setupEmail(maybeProject, form.getEmailField)
  }

  def newAuthorField(maybeProject: Option[Project] = None): JTextField = {
    new JTextField() <| (f => setupAuthor(None, f))
  }

  def setupAuthor(maybeProject: Option[Project], field: JTextField): Unit = {
    maybeSetTextFromCommandLine(
      maybeProject.nullMap(_.getBasePath), field,
      "git", "config", "user.name"
    )
  }

  def setupAuthor(maybeProject: Option[Project], form: AddCabalPackageForm): Unit = {
    setupAuthor(maybeProject, form.getAuthorField)
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

  def setupLicense(form: AddCabalPackageForm): Unit = {
    ("" +: licenses).foreach(form.getLicenseField.addItem)
  }

  def setupCabalVersion(maybeProject: Option[Project], form: AddCabalPackageForm): Unit = {
    for(
      project <- maybeProject;
      cabal <- CabalExecutor.create(project, None).right.toOption;
      version <- cabal.getNumericVersion.right.toOption.flatMap(cleanString)
    )
    form.getCabalVersionField.setText(">=" + version)
  }

  def maybeSetTextFromCommandLine(workDir: Option[String], component: JTextComponent,
                                  command: String, args: String*): Unit = {
    ExecUtil.readCommandLine(workDir.orNull, command, args: _*).fold(
      e => NotificationUtil.displaySimpleNotification(
        NotificationType.ERROR, null, "exec", e.getMessage
      ),
      stdout => maybeSetText(component, stdout)
    )
  }

  def maybeSetText(textComponent: JTextComponent, value: String): Unit = {
    cleanString(value).foreach(textComponent.setText)
  }

  def cleanString(s: String): Option[String] = Option(s).map(_.trim).filter(_.nonEmpty)

  def setupVersion(form: AddCabalPackageForm): Unit = {
    form.getPackageVersionField.setText("0.1.0.0")
  }

  def newComponentTypeField(): SComboBox[CabalComponentType] = {
    new SComboBox[CabalComponentType] <| setupComponentType
  }

  def setupComponentType(field: SComboBox[CabalComponentType]): Unit = {
    Seq(
      CabalComponentType.Executable,
      CabalComponentType.Library
    ).foreach(field.addItem)
    field.setSelectedIndex(0)
  }
  
  def setupComponentType(form: AddCabalPackageForm): Unit = {
    setupComponentType(form.getComponentTypeField)
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

  private def asArg(buildType: CabalComponentType): String = buildType match {
    case CabalComponentType.Executable => "--is-executable"
    case CabalComponentType.Library => "--is-library"
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
