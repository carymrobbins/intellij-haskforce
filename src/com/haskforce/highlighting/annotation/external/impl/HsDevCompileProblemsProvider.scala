package com.haskforce.highlighting.annotation.external.impl

import com.haskforce.highlighting.annotation.external.hsdev.{HsDevExecutor, HsDevModuleLocation, HsDevNote, HsDevOutputMessage}
import com.haskforce.highlighting.annotation.external.{GhcMod, ProblemsProvider}
import com.haskforce.highlighting.annotation.{HaskellProblem, Problems}
import com.haskforce.settings.ToolKey
import com.haskforce.ui.tools.HaskellToolsConsole
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiFile

class HsDevCompileProblemsProvider private(
  hsDevExecutor: HsDevExecutor,
  psiFile: PsiFile,
  toolsConsole: HaskellToolsConsole.Curried
) extends ProblemsProvider {
  override def getProblems: Option[Problems] = {
    toProblems(psiFile, hsDevExecutor.checkContents(psiFile))
  }

  private def toProblems(
    psiFile: PsiFile,
    xs: Vector[HsDevNote[HsDevOutputMessage]]
  ): Option[Problems] = {
    if (xs.isEmpty) {
      None
    } else {
      val res = new Problems(xs.length)

      def addProblem(
        note: HsDevNote[HsDevOutputMessage],
        fileModule: HsDevModuleLocation.FileModule
      ): Unit = {
        val fileName = fileModule.file
        val message =
          note.note.message +
            note.note.suggestion.fold("")("\nSuggestion: " + _)
        val problem = HsDevCompileProblemsProvider.HsDevProblem(
          file_ = fileName,
          startLine_ = note.region.from.line,
          startColumn_ = note.region.from.column,
          endLine = note.region.to.line,
          endColumn = note.region.to.column,
          message_ = message
        )
        res.add(problem)
      }

      xs.foreach { note =>
        note.source match {
          // This is the only case that matters for us.
          case fileModule: HsDevModuleLocation.FileModule => addProblem(note, fileModule)
          case _ => // skip
        }
      }

      Some(res)
    }
  }
}

object HsDevCompileProblemsProvider {

  def create(psiFile: PsiFile): Option[HsDevCompileProblemsProvider] = {
    HsDevExecutor.get(psiFile).map(hsDevExecutor =>
      new HsDevCompileProblemsProvider(
        hsDevExecutor,
        psiFile,
        HaskellToolsConsole.get(psiFile.getProject).curry(ToolKey.HSDEV)
      )
    )
  }

  // TODO: Refactor GhcMod.Problem to somewhere else! Maybe a GhcProblem class?
  // Also, this is sad to subclass it here...
  final case class HsDevProblem(
    // TODO: Using _ suffix to avoid clashing with the java final member
    // This should be better resolved later when the GhcMod.Problem is
    // refactored.
    file_ : String,
    startLine_ : Int,
    startColumn_ : Int,
    endLine: Int,
    endColumn: Int,
    message_ : String
  ) extends GhcMod.Problem(
    file_ ,
    startLine_ ,
    startColumn_ ,
    message_
  ) {
    // TODO: The original GhcMod.Problem just guesses at stop offsets.
    // We actually have them here, so need to clean this up.
    override protected def getTextRange(psiFile: PsiFile): TextRange = {
      val text = psiFile.getText
      TextRange.create(
        // Inlined for clarity from getOffsetStart()
        StringUtil.lineColToOffset(text, startLine - 1, startColumn - 1),
        StringUtil.lineColToOffset(text, endLine - 1, endColumn - 1)
      )
    }
  }
}
