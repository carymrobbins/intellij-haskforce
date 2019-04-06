package com.haskforce.actions

import com.haskforce.HaskellIcons
import com.intellij.ide.actions.{CreateFileFromTemplateAction, CreateFileFromTemplateDialog}
import com.intellij.openapi.ui.InputValidatorEx
import java.util.regex.Pattern
import CreateHaskellFileAction._
import com.intellij.ide.fileTemplates.{FileTemplate, FileTemplateManager, FileTemplateUtil}
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.module.ModuleUtilCore
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.psi.{PsiDirectory, PsiFile}
import scala.collection.mutable

class CreateHaskellFileAction
  extends CreateFileFromTemplateAction(TITLE, DESCRIPTION, HaskellIcons.FILE)
  with DumbAware {

  override def getActionName(directory: PsiDirectory, newName: String, templateName: String): String = TITLE

  override def buildDialog(project: Project, directory: PsiDirectory, builder: CreateFileFromTemplateDialog.Builder): Unit =
    builder
      .setTitle(TITLE)
      .addKind("Empty module", HaskellIcons.FILE, HASKEL_MODULE_TEMPLATE_NAME)
      .setValidator(MyInputValidator)

  override def createFileFromTemplate(name: String, template: FileTemplate, dir: PsiDirectory): PsiFile = {
    val project = dir.getProject
    val qualifiedName = getQualifiedName(name)
    val fileName = buildFileName(qualifiedName)
    val targetDir = createTargetSubDir(qualifiedName, dir)
    val props = FileTemplateManager.getInstance(project).getDefaultProperties
    val moduleName = determineFullyQualifiedModuleName(qualifiedName, dir)
    props.setProperty("HASKELL_MODULE_NAME", moduleName)
    val psiFile = FileTemplateUtil.createFromTemplate(template, fileName, props, targetDir).getContainingFile
    if (psiFile.getVirtualFile != null) FileEditorManager.getInstance(project).openFile(psiFile.getVirtualFile, true)
    psiFile
  }

  private def getQualifiedName(name: String): String =
    if (name.endsWith(".hs")) name.substring(0, name.lastIndexOf('.')) else name

  private def createTargetSubDir(qualifiedName: String, dir: PsiDirectory): PsiDirectory = {
    if (!qualifiedName.contains('.')) return dir
    var targetDir = dir
    qualifiedName.split('.').dropRight(1).foreach { part =>
      val found = targetDir.findSubdirectory(part)
      targetDir = if (found != null) found else targetDir.createSubdirectory(part)
    }
    targetDir
  }

  private def buildFileName(qualifiedName: String): String =
    qualifiedName.substring(qualifiedName.lastIndexOf('.') + 1) + ".hs"

  /** Performs best guess for the fully-qualified module name based on the user-supplied qualifiedName and directory. */
  private def determineFullyQualifiedModuleName(qualifiedName: String, dir: PsiDirectory): String = {
    val module = ModuleUtilCore.findModuleForFile(dir.getVirtualFile, dir.getProject)
    if (module == null) return qualifiedName
    val sourceRoots = ModuleRootManager.getInstance(module).getSourceRoots.map(_.getCanonicalPath).toSet
    val prefix = new mutable.ArrayStack[String]()
    var checkDir: PsiDirectory = dir
    while (!sourceRoots.contains(checkDir.getVirtualFile.getCanonicalPath)) {
      prefix.push(checkDir.getName)
      checkDir = checkDir.getParent
      if (checkDir == null) return qualifiedName
    }
    if (prefix.isEmpty) return qualifiedName
    prefix.mkString(".") + "." + qualifiedName
  }
}

object CreateHaskellFileAction {

  private val TITLE = "New Haskell File"
  private val DESCRIPTION = "Creates a new Haskell source file"
  private val HASKEL_MODULE_TEMPLATE_NAME = "Haskell Module"
  private val VALID_NAME_REGEX = Pattern.compile("^([A-Z][A-Za-z0-9]*)(\\.[A-Z][A-Za-z0-9]*)*(\\.hs)?$")

  private object MyInputValidator extends InputValidatorEx {

    override def checkInput(input: String): Boolean = true

    override def canClose(input: String): Boolean = getErrorText(input) == null

    override def getErrorText(input: String): String = {
      if (input.isEmpty) return null
      if (VALID_NAME_REGEX.matcher(input).matches()) return null
      s"'$input' is not a valid Haskell module name."
    }
  }
}
