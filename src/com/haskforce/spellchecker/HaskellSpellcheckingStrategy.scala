package com.haskforce.spellchecker

import com.haskforce.HaskellLanguage
import com.haskforce.cabal.CabalLanguage
import com.haskforce.psi._
import com.haskforce.utils.CastUtil.Ops
import com.haskforce.utils.PQ
import com.intellij.psi.{PsiComment, PsiElement}
import com.intellij.spellchecker.inspections.PlainTextSplitter
import com.intellij.spellchecker.tokenizer.{SpellcheckingStrategy, Tokenizer, TokenizerBase}

/**
 * Provide spellchecker support for Haskell/Cabal sources.
 */
class HaskellSpellcheckingStrategy extends SpellcheckingStrategy {

  override def getTokenizer(element: PsiElement): Tokenizer[_ <: PsiElement] = {
    // We need to split on apostrophes when spellchecking Haskell identifiers.
    if (isHaskellIdent(element)) {
      HaskellSpellcheckingStrategy.HASKELL_IDENT_TOKENIZER
    } else {
      HaskellSpellcheckingStrategy.STANDARD_TOKENIZER
    }
  }

  override def isMyContext(e: PsiElement): Boolean = {
    (isHaskell(e) || isCabal(e)) && isDefinitionNode(e)
  }

  private def isHaskell(e: PsiElement): Boolean = {
    HaskellLanguage.INSTANCE.is(e.getLanguage)
  }

  private def isHaskellIdent(e: PsiElement): Boolean = {
    e.isInstanceOf[HaskellNamedElement]
  }

  private def isCabal(e: PsiElement): Boolean = {
    CabalLanguage.INSTANCE.is(e.getLanguage)
  }

  private def isDefinitionNode(e: PsiElement): Boolean = {
    e match {

      // Any text in a comment should go through the spellchecker
      case _: PsiComment => true

      // The following are improvements attempt to cover cases that
      // HaskellUtil.definitionNode do not. Ideally, this could eventually be integrated
      // into definitionNode; however, work would need to be done to ensure this plays nicely
      // with callers of definitionNode.

      case x: HaskellConid => (
        isModuleDecl(x)
          || isDataDecl(x)
          || isNewtypeDecl(x)
          || isDataConstrDecl(x)
          || isNewtypeConstrDecl(x)
          || isClassDecl(x)
      )

      case x: HaskellVarid => (
        isGenDecl(x)
          || isFunDecl(x)
          || isClassOrInstanceTypeVar(x)
          || isFuncTypeVar(x)
          || isDataTypeVar(x)
          || isNewtypeTypeVar(x)
      )

      // If we're not in a definition node, we shouldn't make it a typo, it's annoying!
      case _ => false
    }
  }

  private def found[A](ob: Option[A]) = ob.isDefined

  private def isModuleDecl(e: HaskellConid) = found {
    e.getParent.cast[HaskellQconid]
      .flatMap(_.getParent.cast[HaskellModuledecl])
  }

  private def isDataDecl(e: HaskellConid) = found {
    PQ.findFirstParent[HaskellTypee](e)
      .flatMap(_.getParent.cast[HaskellDatadecl])
  }

  private def isNewtypeDecl(e: HaskellConid) = found {
    e.getParent.cast[HaskellTycon]
      .flatMap(_.getParent.cast[HaskellNewtypedecl])
  }

  private def isDataConstrDecl(e: HaskellConid) = found {
    e.getParent.cast[HaskellCon]
      .flatMap(_.getParent.cast[HaskellConstr]
        .flatMap(_.getParent.cast[HaskellDatadecl]))
  }

  private def isNewtypeConstrDecl(e: HaskellConid) = found {
    e.getParent.cast[HaskellCon]
      .flatMap(_.getParent.cast[HaskellNewconstr]
        .flatMap(_.getParent.cast[HaskellNewtypedecl]))
  }

  private def isClassDecl(e: HaskellConid) = {
    PQ.collectFirstParent(e) {
      case x: HaskellCtype if x.getParent.cast[HaskellClassdecl].isDefined => true
    }.getOrElse(false)
  }

  private def isGenDecl(e: HaskellVarid) = found {
    e.getParent.cast[HaskellVars]
      .flatMap(_.getParent.cast[HaskellGendecl])
  }

  private def isFunDecl(e: HaskellVarid) = found {
    e.getParent.cast[HaskellPat]
      .flatMap(_.getParent.cast[HaskellFunorpatdecl]
        // Filters out functions defined in `instance` blocks
        .filter(_.getParent.cast[HaskellIdecl].isEmpty))
  }

  private def isClassOrInstanceTypeVar(e: HaskellVarid) = {
    PQ.collectFirstParent(e) {
      case x: HaskellCtype if x.getParent.cast[HaskellClassdecl].isDefined => true
      case x: HaskellCtype if x.getParent.cast[HaskellInstancedecl].isDefined => true
    }.getOrElse(false)
  }

  private def isFuncTypeVar(e: HaskellVarid) = found {
    e.getParent.cast[HaskellTyvar]
      .flatMap(PQ.findFirstParent[HaskellGendecl])
  }

  private def isDataTypeVar(e: HaskellVarid) = found {
    e.getParent.cast[HaskellTyvar]
      .flatMap(_.getParent.cast[HaskellAtype]
        .flatMap(_.getParent.cast[HaskellTypee]
          .flatMap(_.getParent.cast[HaskellDatadecl])))
  }

  private def isNewtypeTypeVar(e: HaskellVarid) = found {
    e.getParent.cast[HaskellTyvar]
      .flatMap(_.getParent.cast[HaskellNewtypedecl])
  }
}

object HaskellSpellcheckingStrategy {

  private val HASKELL_IDENT_TOKENIZER = new TokenizerBase[PsiElement](
    HaskellSpellcheckingSplitter.getInstance()
  )

  private val STANDARD_TOKENIZER = new TokenizerBase[PsiElement](
    PlainTextSplitter.getInstance()
  )
}
