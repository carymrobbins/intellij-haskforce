package com.haskforce.haskell.lang.parameterInfo

import com.haskforce.highlighting.annotation.external.TypeInfoUtil
import com.haskforce.psi._
import com.haskforce.psi.impl.HaskellElementFactory
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.lang.parameterInfo._
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.{Editor, VisualPosition}
import com.intellij.openapi.module.ModuleUtilCore
import com.intellij.psi.{PsiElement, PsiWhiteSpace}
import com.intellij.psi.util.PsiTreeUtil

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class HaskellParameterInfoHandler extends ParameterInfoHandler[PsiElement, HaskellTypee] {

  import HaskellParameterInfoHandler.{LOG, PARAM_INDEX_KEY}

  override def couldShowInLookup: Boolean = true

  override def getParametersForLookup(item: LookupElement, context: ParameterInfoContext): Array[AnyRef] = {
    LOG.debug("NOT IMPL: getParametersForLookup")
    Array.empty
  }

  private def findElement(context: ParameterInfoContext): PsiElement = {
    context.getFile.findElementAt(context.getOffset)
  }

  override def findElementForParameterInfo(context: CreateParameterInfoContext): PsiElement = {
    findElement(context)
  }

  override def findElementForUpdatingParameterInfo(context: UpdateParameterInfoContext): PsiElement = {
    findElement(context)
  }

  private def findFunction(element: PsiElement): Option[PsiElement] = {
    _findFunctionAndOrArgIndex(element, findArgIndex = false).map(_._1)
  }

  private def findFunctionAndArgIndex(element: PsiElement): Option[(PsiElement, Option[Int])] = {
    _findFunctionAndOrArgIndex(element, findArgIndex = true)
  }

  /** If `element` is whitespace, find the previous leaf element; otherwise, just return element; e.g.
    * `fo[caret]o` should return the element associated with `foo`
    * `foo [caret]` should return the element associated with `foo`
    * `foo bar [caret]` should return the element associated with `bar`
    */
  private def approximateElement(element: PsiElement): PsiElement = element match {
    case _: PsiWhiteSpace => approximateElement(PsiTreeUtil.prevVisibleLeaf(element))
    case _ => element
  }

  private def _findFunctionAndOrArgIndex(input: PsiElement, findArgIndex: Boolean): Option[(PsiElement, Option[Int])] = {
    // TODO: This needs to be smarter and find the right function call, meaning skipping
    // operators, only checking for QVAR, EXP, etc.

    // May be a whitespace element, so we will attempt to approximate the user's "current" element.
    val element = approximateElement(input)

    val exp: HaskellExp = PsiTreeUtil.findFirstParent(element, e => e.isInstanceOf[HaskellExp]) match {
      case x: HaskellExp => x
      case _ => return None
    }
    val (func: PsiElement, params: List[PsiElement]) = exp.getChildren.toList match {
      case x :: xs => (x, xs)
      case _ => return None
    }

    if (!findArgIndex) return Some((func, None))

    val optIndex = params.zipWithIndex
      .find(x => PsiTreeUtil.isAncestor(x._1, element, true))
      .map(_._2)

    Some((func, optIndex))
  }

  override def showParameterInfo(element: PsiElement, context: CreateParameterInfoContext): Unit = {

    // TODO: We can start with resolving the reference and fall back to ghc-mod only if we have to

    val module = ModuleUtilCore.findModuleForPsiElement(element)
    if (module == null) return LOG.debug(s"Failed to find module for element")
    val vFile = context.getFile.getVirtualFile
    if (vFile == null) return LOG.debug(s"Failed to find virtual file for psi file")
    val func = findFunction(element).getOrElse { return LOG.debug("Function element not found") }
    val (funcStartPos, funcEndPos) = getElementVisualRange(context.getEditor, func)
    val typeInfoStr = TypeInfoUtil.getTypeInfo(module, funcStartPos, funcEndPos, vFile)
    if (typeInfoStr == null) return LOG.debug("Failed to get type info")
    // getTypeInfo is bad and returns a String error message on failure, so we will do a dumb check
    // for "->" to see if we actually got a function type back. Very, very poor. :(
    if (!typeInfoStr.contains("->")) return LOG.debug("Type does not have ->")
    val ctype = HaskellElementFactory.createCtypeFromText(context.getProject, typeInfoStr)
    if (ctype == null) return LOG.debug("Failed to parse ctype from type response")
    val paramElements = gatherParamTypeElements(ctype)
    if (paramElements.isEmpty) return LOG.debug("No param type elements found")

    context.setItemsToShow(paramElements.toArray)
    context.showHint(element, element.getTextRange.getStartOffset, this)
  }

  private def gatherParamTypeElements(ctype: HaskellCtype): Vector[HaskellTypee] = {
    @tailrec
    def loop(acc: Vector[HaskellTypee], i: Int, typee: HaskellTypee): Vector[HaskellTypee] = {
      if (typee == null) return acc
      val next = typee.getTypee
      if (next != null) typee.putUserData(PARAM_INDEX_KEY, i)
      loop(acc :+ typee, i + 1, next)
    }
    loop(Vector.empty, 0, ctype.getTypee)
  }

  /** Adapted from com.intellij.openapi.editor.impl.CaretImpl.getSelectionStartPosition */
  private def getElementVisualRange(editor: Editor, element: PsiElement): (VisualPosition, VisualPosition) = {
    val range = element.getTextRange
    val start = editor.offsetToVisualPosition(range.getStartOffset, true, false)
    val end = editor.offsetToVisualPosition(range.getEndOffset, false, true)
    (TypeInfoUtil.correctFor0BasedVS1Based(start), TypeInfoUtil.correctFor0BasedVS1Based(end))
  }

  override def updateParameterInfo(parameterOwner: PsiElement, context: UpdateParameterInfoContext): Unit = {
    for {
      x <- findFunctionAndArgIndex(parameterOwner)
      i <- x._2
    } yield context.setCurrentParameter(i)
  }

  override def updateUI(p: HaskellTypee, context: ParameterInfoUIContext): Unit = {
    val start = 0
    val end = 0
    val optIndex = Option(p.getUserData(PARAM_INDEX_KEY))
    val prefix = optIndex match {
      case Some(0) => "::"
      case _ => "→"
    }
    val paramText = prefix + " " + p.getAtypeList.asScala.map(_.getText).mkString(" ")
    val isCurrentParameter = optIndex.contains(context.getCurrentParameterIndex)
    val disabled = !isCurrentParameter

    context.setupUIComponentPresentation(
      paramText, start, end, disabled, false, true, context.getDefaultParameterColor
    )
  }

  override def getParametersForDocumentation(p: HaskellTypee, context: ParameterInfoContext): Array[AnyRef] = {
    LOG.debug("NOT IMPL: getParametersForDocumentation")
    Array.empty
  }

  override def getParameterCloseChars: String = " \r\n\t"

  override def tracksParameterIndex(): Boolean = {
    LOG.debug("NOT IMPL: tracksParameterIndex")
    true
  }
}

object HaskellParameterInfoHandler {
  private val LOG = Logger.getInstance(classOf[HaskellParameterInfoHandler])
  private val PARAM_INDEX_KEY = new com.intellij.openapi.util.Key[Int]("param-index")
}
