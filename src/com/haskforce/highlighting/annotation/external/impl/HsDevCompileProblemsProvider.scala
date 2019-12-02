package com.haskforce.highlighting.annotation.external.impl

import com.haskforce.highlighting.annotation.external.hsdev.{HsDevExecutor, HsDevNote, HsDevOutputMessage}
import com.haskforce.highlighting.annotation.external.{GhcMod, ProblemsProvider}
import com.haskforce.highlighting.annotation.{HaskellProblem, Problems}
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiFile

class HsDevCompileProblemsProvider private(
  hsDevExecutor: HsDevExecutor,
  psiFile: PsiFile
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
      val res = new Problems(new Array[HaskellProblem](xs.length))
      xs.zipWithIndex.foreach { case (note, i) =>
        // TODO: A bit of a hack but simpler than matching on note.source?
        val fileName = psiFile.getName
        val message = note.note.suggestion match {
          case None => note.note.message
          case Some(suggestion) => note.note.message + "\nSuggestion: " + suggestion
        }
        val problem = new HsDevCompileProblemsProvider.HsDevProblem(
          file = fileName,
          startLine = note.region.from.line,
          startColumn = note.region.from.column,
          endLine = note.region.to.line,
          endColumn = note.region.to.column,
          message = message
        )
        res.set(i, problem)
      }
      Some(res)
    }
  }
}

object HsDevCompileProblemsProvider {

  def create(psiFile: PsiFile): Option[HsDevCompileProblemsProvider] = {
    HsDevExecutor.get(psiFile).map(new HsDevCompileProblemsProvider(_, psiFile))
  }

  // TODO: Refactor GhcMod.Problem to somewhere else! Maybe a GhcProblem class?
  // Also, this is sad to subclass it here...
  class HsDevProblem(
    file: String,
    startLine: Int,
    startColumn: Int,
    endLine: Int,
    endColumn: Int,
    message: String
  ) extends GhcMod.Problem(
    file,
    startLine,
    startColumn,
    message
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
