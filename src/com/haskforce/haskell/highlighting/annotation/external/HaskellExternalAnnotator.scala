package com.haskforce.highlighting.annotation.external

import scala.collection.JavaConverters._

import com.intellij.lang.annotation.{AnnotationHolder, ExternalAnnotator}
import com.intellij.openapi.application.{ApplicationManager, ModalityState}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.psi.PsiFile
import org.jetbrains.annotations.{NotNull, Nullable}

import com.haskforce.Implicits._
import com.haskforce.haskell.codeInsight.HaskellCompletionContributor
import com.haskforce.highlighting.annotation.{HaskellAnnotationHolder, HaskellProblem, Problems}
import HaskellExternalAnnotator.State
import com.haskforce.utils.{SAMUtils, WrappedFuture}

/** Single annotator that calls all external tools used for annotations. */
class HaskellExternalAnnotator extends ExternalAnnotator[PsiFile, State] {

  /**
   * The default implementation here is to not annotate files that have lexer/parser errors.
   * This is kind of lame since the error shouldn't prevent our external annotations from
   * appearing.  To get around this, we ignore the `hasErrors` argument.
   */
  @Nullable
  override def collectInformation
      (@NotNull file: PsiFile, @NotNull editor: Editor, hasErrors: Boolean): PsiFile = {
    collectInformation(file)
  }

  /** Simply returns the file so it will get passed to doAnnotate() */
  @NotNull
  override def collectInformation(@NotNull file: PsiFile): PsiFile = {
    file
  }

  /** Builds the set of annotations to be applied to the source file. */
  @Nullable
  override def doAnnotate(@NotNull file: PsiFile): State = {
    // We need to save the files so external processes will see what we see.
    saveAllFiles()
    // Constructs our annotation state for a file given our registered providers.
    State.buildForFile(file)
  }

  /** Applies the annotations from the State to the source file. */
  override def apply
      (@NotNull file: PsiFile, state: State, @NotNull holder: AnnotationHolder)
      : Unit = {
    HaskellExternalAnnotator.createAnnotations(
      file, state.getProblems, new HaskellAnnotationHolder(holder)
    )
  }

  private def saveAllFiles(): Unit = {
    ApplicationManager.getApplication.invokeAndWait(
      () => FileDocumentManager.getInstance.saveAllDocuments(),
      ModalityState.any()
    )
  }
}

object HaskellExternalAnnotator {

  /** Registered problem providers for our configured tools. */
  val problemsProviderFactories: List[ProblemsProviderFactory] = List(
    LintProblemsProviderFactory,
    CompileProblemsProviderFactory
  )

  /** Helper to annotate problems in our source from external tools. */
  def createAnnotations
      (@NotNull file: PsiFile,
       problems: Stream[HaskellProblem],
       @NotNull holder: HaskellAnnotationHolder)
      : Unit = {
    if (!file.isValid) return
    problems.foreach { _.createAnnotations(file, holder) }
  }

  /** Wraps the problems provided by external tools. */
  final case class State(problems: List[Option[Problems]]) {

    /** Streams the problems in the order they were provided to the State. */
    def getProblems: Stream[HaskellProblem] = {
      problems.toStream.flatten.flatMap(_.iterator().asScala.toStream)
    }
  }

  object State {
    /** Smart constructor which traverses our factories and obtains problem futures. */
    def buildForFile(file: PsiFile): State = {
      State(problemsProviderFactories.flatMap(_.get(file).map(_.getProblems)))
    }
  }
}
