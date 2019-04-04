package com.haskforce.highlighting.annotation.external

import com.haskforce.utils.ToEitherObjectOps._
import com.intellij.openapi.editor.{Editor, LogicalPosition}
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.intellij.psi.util.PsiUtilBase

/**
  * Generalized factory for getting a configured [[TypeInfoProvider]]
  * in [[com.haskforce.actions.TypeInfoAction]].
  */
trait TypeInfoProviderFactory {
  def get(input: TypeInfoProviderFactory.Input): Option[TypeInfoProvider]
}

object TypeInfoProviderFactory {

  final case class Input(
    editor: Editor,
    psiFile: PsiFile,
    canonicalFilePath: String,
    selectionStart: LogicalPosition,
    selectionStop: LogicalPosition,
    offsetStart: Int,
    offsetStop: Int
  )

  def mkInput(project: Project): Either[String, Input] = {
    for {
      editor <- Either.nonNull(
        FileEditorManager.getInstance(project).getSelectedTextEditor,
        "Failed to get text editor"
      )
      psiFile <- Either.nonNull(
        PsiUtilBase.getPsiFileInEditor(editor, project),
        "Failed to get psi file"
      )
      virtualFile <- Either.nonNull(
        psiFile.getVirtualFile,
        "Failed to get virtual file"
      )
      canonicalFilePath <- Either.nonNull(
        virtualFile.getCanonicalPath,
        "Failed to get canonical path"
      )
      selection = editor.getSelectionModel
      offsetStart = selection.getSelectionStart
      offsetEnd = selection.getSelectionEnd
      selectionStart = editor.offsetToLogicalPosition(offsetStart)
      selectionStop = editor.offsetToLogicalPosition(offsetEnd)
    } yield Input(
      editor,
      psiFile,
      canonicalFilePath,
      selectionStart,
      selectionStop,
      offsetStart,
      offsetEnd
    )
  }
}
