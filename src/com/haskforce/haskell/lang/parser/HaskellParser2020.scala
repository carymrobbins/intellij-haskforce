package com.haskforce.haskell.lang.parser

import java.util

import com.haskforce.HaskellLanguage
import com.haskforce.haskell.lang.parser.HaskellParser2020.Psi.HElement
import com.haskforce.haskell.lang.parser.{HaskellTokenTypes2020 => T}
import com.haskforce.psi.HaskellTokenType
import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.impl.PsiBuilderAdapter
import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.psi.{PsiElement, TokenType}
import com.intellij.psi.tree.IElementType
import com.intellij.psi.util.PsiTreeUtil

import scala.reflect.ClassTag

final class HaskellParser2020 extends PsiParser {
  override def parse(root: IElementType, builder: PsiBuilder): ASTNode = {
    new HaskellPsiBuilder(builder).parseRoot(root)
  }
}

private final class HaskellPsiBuilder(builder: PsiBuilder) extends PsiBuilderAdapter(builder) {

  import HaskellParser2020.{Elements => E}
  import HaskellPsiBuilder.{Parse, withMark, parseIfToken}

  private implicit val self: HaskellPsiBuilder = this

  def parseRoot(root: IElementType): ASTNode = {
    val rootMarker = mark()
    pModule.run()
    remapUnparsedWhitespace()
    handleUnparsedInput()
    rootMarker.done(root)
    getTreeBuilt
  }

  private def remapUnparsedWhitespace(): Unit = {
    if (eof()) return
    while (isSyntheticWhitespace(getTokenType)) {
      remapCurrentToken(TokenType.WHITE_SPACE)
      advanceLexer()
    }
  }

  private def isSyntheticWhitespace(t: IElementType): Boolean = t match {
    case T.WHITESPACELBRACETOK => true
    case T.WHITESPACERBRACETOK => true
    case T.WHITESPACESEMITOK => true
    case _ => false
  }

  private def handleUnparsedInput(): Unit = {
    if (eof()) return
    withMark { m =>
      while (!eof()) advanceLexer()
      m.done(E.UNKNOWN)
    }
  }

  private def pModule = Parse {
    withMark { m =>
      pModuleDecl.run()
      m.done(E.MODULE)
    }.isDone
  }

  private def pModuleDecl = parseIfToken(T.MODULETOKEN) { m =>
    pModuleName.run()
    pModuleExports.run()
    if (getTokenType == T.WHERE) advanceLexer()
    m.done(E.MODULE_DECL)
  }

  private def pModuleName = Parse {
    withMark { m =>
      if (pQConid.run()) {
        m.done(E.MODULE_NAME)
      } else {
        m.rollbackTo()
      }
    }.isDone
  }

  private def pModuleExports = Parse {
    // TODO: Might be nicer to interpret "()" in the lexer as
    // LPAREN and RPAREN and interpret that sequence of tokens, when
    // encountered where we expect a Conid, as a Conid. This way we
    // don't have to have hacks like this.
    // Luckily, "( )" in the lexer _will_ be interpreted as
    // LPAREN PsiWhiteSpace RPAREN, so we don't need to special case that
    // here.
    if (getTokenText == "()") {
      val m = mark()
      advanceLexer() // skip the "()" token
      m.done(E.MODULE_EXPORTS)
      true
    } else if (getTokenType != T.LPAREN) {
      false
    } else {
      withMark { m =>
        advanceLexer() // skip seen LPAREN
        while (getTokenType != T.RPAREN) {
          // skip commas
          while (getTokenType == T.COMMA) advanceLexer()
          pModuleExport.run()
          // skip commas
          while (getTokenType == T.COMMA) advanceLexer()
        }
        m.done(E.MODULE_EXPORTS)
      }.isDone
    }
  }

  private def pModuleExport = {
    pModuleExportModule
      .orElse(pModuleExportTyCon)
  }

  private def pModuleExportModule = parseIfToken(T.MODULETOKEN) { m =>
    if (!pModuleName.run()) error("Missing module name")
    m.done(E.MODULE_EXPORT_MODULE)
  }

  private def pModuleExportTyCon = Parse {
    withMark { m =>
      if (!pQTyCon.run()) {
        m.rollbackTo()
      } else {
        m.done(E.MODULE_EXPORT_TYCON)
      }
    }.isDone
  }

  private def pTyCon = Parse {
    withMark { m =>
      if (pConid.run()) {
        m.done(E.TYCON_CONID)
      } else {
        m.rollbackTo()
      }
    }.orElse { m =>
      if (pConsym.run()) {
        m.done(E.TYCON_CONSYM)
      } else {
        m.rollbackTo()
      }
    }.isDone
  }

  private def pQTyCon = pQualified(pTyCon, E.QTYCON)

  private def pConid = pTokenAs(T.CONIDREGEXP, E.CONID)

  private def pQConid = pQualified(pConid, E.QCONID)

  private def pConsym = pTokenAs(T.CONSYMTOK, E.CONSYM)

  private def pQConsym = pQualified(pConsym, E.QCONSYM)

  private def pQualified(p: Parse, e: E.HElementType) = Parse {
    withMark { m =>
      pQualifiedPrefix.run() // ok to ignore result
      if (p.run()) {
        m.done(e)
      } else {
        m.rollbackTo()
      }
    }.isDone
  }

  private def pQualifiedPrefix = Parse {
    def hasMore = getTokenType == T.CONIDREGEXP && lookAhead(1) == T.PERIOD
    if (!hasMore) {
      false
    } else {
      withMark { m =>
        do {
          assert(pConid.run())
          assert(getTokenType == T.PERIOD)
          advanceLexer()
        } while (hasMore)
        m.done(E.QUALIFIED_PREFIX)
      }.isDone
    }
  }

  private def pTokenAs(t: HaskellTokenType, e: E.HElementType) = Parse {
    if (getTokenType != t) {
      false
    } else {
      val m = mark()
      advanceLexer()
      m.done(e)
      true
    }
  }
}

object HaskellPsiBuilder {

  final class Parse(val run: () => Boolean) extends AnyVal {
    def orElse(p: Parse): Parse = Parse { run() || p.run() }
  }

  object Parse {
    def apply(run: => Boolean): Parse = new Parse(() => run)
  }

  type WithMark = (Marker => MarkResult) => MarkResult

  def withMark(f: Marker => MarkResult)(implicit b: HaskellPsiBuilder): MarkResult = {
    f(new Marker(b.mark()))
  }

  def parseIf(p: => Boolean)(f: Marker => MarkResult)(implicit b: HaskellPsiBuilder): Parse = Parse {
    if (!p) {
      false
    } else {
      withMark(f).isDone
    }
  }

  def parseIfToken(t: HaskellTokenType)(f: Marker => MarkResult)(implicit b: HaskellPsiBuilder): Parse = {
    parseIf(b.getTokenType == t) { m =>
      b.advanceLexer() // skip the checked token
      f(m)
    }
  }

  final class Marker(private val m: PsiBuilder.Marker) extends AnyVal {
    def done(t: HaskellParser2020.Elements.HElementType): MarkResult = {
      m.done(t)
      MarkResult.Done
    }

    def rollbackTo(): MarkResult = {
      m.rollbackTo()
      MarkResult.Rollback
    }
  }

  sealed trait MarkResult {
    def isDone: Boolean
    def orElse(f: Marker => MarkResult)(implicit b: HaskellPsiBuilder): MarkResult
  }
  object MarkResult {
    case object Done extends MarkResult {
      override def isDone: Boolean = true
      override def orElse(f: Marker => MarkResult)(implicit b: HaskellPsiBuilder): MarkResult = {
        this
      }
    }
    case object Rollback extends MarkResult {
      override def isDone: Boolean = false
      override def orElse(f: Marker => MarkResult)(implicit b: HaskellPsiBuilder): MarkResult = {
        withMark(f)
      }
    }
  }
}

object HaskellParser2020 {

  object Elements {
    sealed class HElementType(name: String) extends IElementType(name, HaskellLanguage.INSTANCE)
    object UNKNOWN extends HElementType("UNKNOWN")
    object MODULE extends HElementType("MODULE")
    object MODULE_DECL extends HElementType("MODULE_DECL")
    object MODULE_NAME extends HElementType("MODULE_NAME")
    object MODULE_EXPORTS extends HElementType("MODULE_EXPORTS")
    object MODULE_EXPORT_MODULE extends HElementType("MODULE_EXPORT_MODULE")
    object MODULE_EXPORT_TYCON extends HElementType("MODULE_EXPORT_TYCON")
    object MODULE_EXPORT_VAR extends HElementType("MODULE_EXPORT_VAR")
    object QUALIFIED_PREFIX extends HElementType("QUALIFIED_PREFIX")
    object CONID extends HElementType("CONID")
    object QCONID extends HElementType("QCONID")
    object CONSYM extends HElementType("CONSYM")
    object QCONSYM extends HElementType("QCONSYM")
    object TYCON_CONID extends HElementType("TYCON_CONID")
    object TYCON_CONSYM extends HElementType("TYCON_CONSYM")
    object QTYCON extends HElementType("QTYCON")
  }

  object Psi {
    trait HElement extends PsiElement

    trait Unknown extends HElement

    trait Module extends HElement {
      def getModuleDecl: Option[ModuleDecl]
    }

    trait ModuleDecl extends HElement {
      def getModuleName: Option[ModuleName]
    }

    trait ModuleName extends HElement {
      def getQConid: QConid
    }

    trait ModuleExports extends HElement {
      def getExports: util.List[ModuleExport]
    }

    sealed trait ModuleExport extends HElement

    trait ModuleExportModule extends ModuleExport {
      def getModuleName: ModuleName
    }

    trait ModuleExportTyCon extends ModuleExport {
      def getQTyCon: QTyCon
      def getDoublePeriod: Option[PsiElement]
      final def exportsAllMembers: Boolean = getDoublePeriod.isDefined
      def getExportedMembers: util.List[QVar]
    }

    trait ModuleExportVar extends ModuleExport {
      def getQVar: QVar
    }

    trait QualifiedPrefix extends HElement {
      def getConids: util.List[Conid]
    }

    trait QConid extends HElement {
      def getQualifiedPrefix: Option[QualifiedPrefix]
      def getConid: Conid
    }

    trait Conid extends HElement {
      def getConidRegexp: PsiElement
    }

    trait Consym extends HElement {
      def getConsymTok: PsiElement
    }

    trait QVar extends HElement {
      def getQualifiedPrefix: Option[QualifiedPrefix]
      def getVar: Var
    }

    sealed trait Var extends HElement

    trait Varid extends Var {
      def getVaridRegexp: PsiElement
    }

    trait Varsym extends Var {
      def getVarsymTok: PsiElement
    }

    trait QTyCon extends HElement {
      def getQualifiedPrefix: Option[QualifiedPrefix]
      def getTyCon: TyCon
    }

    sealed trait TyCon extends HElement

    trait TyConConid extends TyCon {
      def getConid: Conid
    }

    trait TyConConsym extends TyCon {
      def getConsym: Consym
    }
  }

  object PsiImpl {

    abstract class HElementImpl(node: ASTNode) extends ASTWrapperPsiElement(node) with HElement {
      override def toString: String = node.getElementType.toString

      protected def one[A <: HElement](implicit ct: ClassTag[A]): A = {
        notNullChild(PsiTreeUtil.getChildOfType[A](this, cls[A]))
      }

      protected def option[A <: HElement](implicit ct: ClassTag[A]): Option[A] = {
        Option(PsiTreeUtil.getChildOfType[A](this, cls[A]))
      }

      protected def list[A <: HElement](implicit ct: ClassTag[A]): util.List[A] = {
        PsiTreeUtil.getChildrenOfTypeAsList[A](this, cls[A])
      }

      protected def oneTok(t: HaskellTokenType): PsiElement = {
        notNullChild(findChildByType(t))
      }

      protected def optionTok(t: HaskellTokenType): Option[PsiElement] = {
        Option(findChildByType(t))
      }
    }

    private def cls[A](implicit ct: ClassTag[A]): Class[A] = {
      ct.runtimeClass.asInstanceOf[Class[A]]
    }

    class UnknownImpl(node: ASTNode) extends HElementImpl(node) with Psi.Unknown

    class ModuleImpl(node: ASTNode) extends HElementImpl(node) with Psi.Module {
      def getModuleDecl: Option[Psi.ModuleDecl] = option
    }

    class ModuleDeclImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleDecl {
      def getModuleName: Option[Psi.ModuleName] = option
    }

    class ModuleNameImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleName {
      def getQConid: Psi.QConid = one
    }

    class ModuleExportsImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExports {
      def getExports: util.List[Psi.ModuleExport] = list
    }

    class ModuleExportModuleImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExportModule {
      def getModuleName: Psi.ModuleName = one
    }

    class ModuleExportTyConImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExportTyCon {
      def getQTyCon: Psi.QTyCon = one
      def getDoublePeriod: Option[PsiElement] = optionTok(T.DOUBLEPERIOD)
      def getExportedMembers: util.List[Psi.QVar] = list
    }

    class ModuleExportVarImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExport {
      def getQVar: Psi.QVar = one
    }

    class QualifiedPrefixImpl(node: ASTNode) extends HElementImpl(node) with Psi.QualifiedPrefix {
      def getConids: util.List[Psi.Conid] = list
    }

    class QConidImpl(node: ASTNode) extends HElementImpl(node) with Psi.QConid {
      def getQualifiedPrefix: Option[Psi.QualifiedPrefix] = option
      def getConid: Psi.Conid = one
    }

    class ConidImpl(node: ASTNode) extends HElementImpl(node) with Psi.Conid {
      def getConidRegexp: PsiElement = oneTok(T.CONIDREGEXP)
    }

    class ConsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.Consym {
      def getConsymTok: PsiElement = oneTok(T.CONSYMTOK)
    }

    class QVarImpl(node: ASTNode) extends HElementImpl(node) with Psi.QVar {
      def getQualifiedPrefix: Option[Psi.QualifiedPrefix] = option
      def getVar: Psi.Var = one
    }

    class VaridImpl(node: ASTNode) extends HElementImpl(node) with Psi.Varid {
      def getVaridRegexp: PsiElement = oneTok(T.VARIDREGEXP)
    }

    class VarsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.Varsym {
      def getVarsymTok: PsiElement = oneTok(T.VARSYMTOK)
    }

    class QTyConImpl(node: ASTNode) extends HElementImpl(node) with Psi.QTyCon {
      def getQualifiedPrefix: Option[Psi.QualifiedPrefix] = option
      def getTyCon: Psi.TyCon = one
    }

    class TyConConidImpl(node: ASTNode) extends HElementImpl(node) with Psi.TyConConid {
      def getConid: Psi.Conid = one
    }

    class TyConConsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.TyConConsym {
      def getConsym: Psi.Consym = one
    }
  }

  object Factory {
    def createElement(node: ASTNode): PsiElement = {
      node.getElementType match {
        case t: Elements.HElementType => createHElement(node, t)
        case t => throw new AssertionError(s"Unexpected element type: $t")
      }
    }

    private def createHElement(node: ASTNode, t: Elements.HElementType): Psi.HElement = {
      t match {
        case Elements.UNKNOWN => new PsiImpl.UnknownImpl(node)
        case Elements.MODULE => new PsiImpl.ModuleImpl(node)
        case Elements.MODULE_DECL => new PsiImpl.ModuleDeclImpl(node)
        case Elements.MODULE_NAME => new PsiImpl.ModuleNameImpl(node)
        case Elements.MODULE_EXPORTS => new PsiImpl.ModuleExportsImpl(node)
        case Elements.MODULE_EXPORT_MODULE => new PsiImpl.ModuleExportModuleImpl(node)
        case Elements.MODULE_EXPORT_TYCON => new PsiImpl.ModuleExportTyConImpl(node)
        case Elements.MODULE_EXPORT_VAR => new PsiImpl.ModuleExportVarImpl(node)
        case Elements.QUALIFIED_PREFIX => new PsiImpl.QualifiedPrefixImpl(node)
        case Elements.CONID => new PsiImpl.ConidImpl(node)
        case Elements.QCONID => new PsiImpl.QConidImpl(node)
        case Elements.CONSYM => new PsiImpl.ConsymImpl(node)
        // TODO case Elements.QCONSYM => new PsiImpl.QConsymImpl(node)
        case Elements.TYCON_CONID => new PsiImpl.TyConConidImpl(node)
        case Elements.TYCON_CONSYM => new PsiImpl.TyConConsymImpl(node)
        case Elements.QTYCON => new PsiImpl.QTyConImpl(node)
        case _ => throw new AssertionError(s"Unexpected Haskell element type '$t' for node '$node'")
      }
    }
  }
}
