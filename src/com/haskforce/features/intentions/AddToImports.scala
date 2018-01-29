package com.haskforce.features.intentions

import com.haskforce.highlighting.annotation.external.{SymbolImportProvider, SymbolImportProviderFactory}
import com.haskforce.psi.{HaskellBody, HaskellImpdecl, HaskellImportt}
import com.haskforce.psi.impl.HaskellElementFactory
import com.haskforce.utils.NotificationUtil
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.notification.NotificationType
import com.intellij.openapi.command.WriteCommandAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.popup.JBPopupFactory
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.ui.components.JBList
import com.intellij.util.IncorrectOperationException
import scala.collection.JavaConverters._

class AddToImports(val symbolName: String) extends BaseIntentionAction {
  override def getFamilyName = "Add to import list"

  override def getText: String = "Import symbol " + symbolName

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean = {
    SymbolImportProviderFactory.get(file).isDefined
  }

  @throws[IncorrectOperationException]
  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {

    val provider = SymbolImportProviderFactory.get(file).get

    val results = provider.findImport(symbolName)

    if (results.isEmpty) {
      NotificationUtil.displaySimpleNotification(
        NotificationType.INFORMATION, project, "Not found",
        s"Could not find any import for the symbol $symbolName"
      )
      return
    }

    val list = new JBList(results: _*)

    val popup = JBPopupFactory.getInstance()
      .createListPopupBuilder(list)
      .setTitle("Identifier to Import")
      .setItemChoosenCallback(() => doAddImport(project, file, list.getSelectedValue))
      .createPopup()

    // TODO: Can we put it at the cursor offset?
    popup.showInFocusCenter()
  }

  def doAddImport(project: Project, file: PsiFile, r: SymbolImportProvider.Result): Unit = {
    WriteCommandAction.runWriteCommandAction(project, { () =>
      val importName = r.importText
      val moduleList = importName.split("\\.").toList
      val imports = PsiTreeUtil.findChildrenOfType(file, classOf[HaskellImpdecl]).asScala
      val addedToExistingImport = imports.foldLeft(false) {
        case (true, _) => true // Short circuit
        case (false, impDecl) =>
          lazy val importedModule = impDecl.getQconidList.asScala.flatMap(_.getConidList.asScala.map(_.getName)).toList
          if (impDecl.getQualified == null
            && impDecl.getRparen != null
            && impDecl.getAs == null
            && impDecl.getHiding == null
            && moduleList == importedModule
          ) { // We already import some qualified symbols, thus we append
            AddToImports.appendToExistingImport(project, symbolName, impDecl)
            true
          }
          else false
      }
      if (!addedToExistingImport) {
        AddToImports.createNewImport(file, project, importName, symbolName, imports)
      }
    }: Runnable)
  }
}

object AddToImports {

  def appendToExistingImport(project: Project, symbolName: String, impDecl: HaskellImpdecl): Unit = {
    val varName: PsiElement = HaskellElementFactory.createVaridFromText(project, symbolName)
    val importt: HaskellImportt = impDecl.getImporttList.asScala.head
    val comma = HaskellElementFactory.createComma(project)
    val space = HaskellElementFactory.createSpace(project)
    val rParen = impDecl.getRparen
    importt.addBefore(comma, rParen)
    importt.addBefore(space, rParen)
    importt.addBefore(varName, rParen)
  }

  def createNewImport(file: PsiFile, project: Project, importName: String, symbolName: String, imports: Iterable[HaskellImpdecl]): Unit = {
    val textImport = s"import $importName ($symbolName)"
    val impDecl = HaskellElementFactory.createImpdeclFromText(project, textImport)
    val body = PsiTreeUtil.getChildOfType(file, classOf[HaskellBody])
    val newline = HaskellElementFactory.createNewLine(project)
    if (imports.nonEmpty) {
      body.addAfter(newline, imports.last)
      body.addAfter(impDecl, imports.last.getNextSibling)
    }
    else {
      val firstChild = body.getFirstChild
      body.addBefore(impDecl, firstChild)
      body.addBefore(newline, firstChild)
    }
  }

}
