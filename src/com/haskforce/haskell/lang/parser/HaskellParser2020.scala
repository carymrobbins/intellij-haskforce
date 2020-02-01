package com.haskforce.haskell.lang.parser

import com.haskforce.HaskellLanguage
import com.intellij.lang.impl.PsiBuilderAdapter
import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.IElementType
import com.haskforce.haskell.lang.parser.{HaskellTokenTypes2020 => T}
import com.haskforce.psi.HaskellTokenType

final class HaskellParser2020 extends PsiParser {
  override def parse(root: IElementType, builder: PsiBuilder): ASTNode = {
    new HaskellPsiBuilder(builder).parseRoot(root)
  }
}

private final class HaskellPsiBuilder(builder: PsiBuilder) extends PsiBuilderAdapter(builder) {

  import HaskellParser2020.{Elements => E}
  import HaskellPsiBuilder.{Parse, withMark}

  private implicit val self: HaskellPsiBuilder = this

  def parseRoot(root: IElementType): ASTNode = {
    val rootMarker = mark()
    pModule.run()
    rootMarker.done(root)
    getTreeBuilt
  }

  private def pModule = Parse {
    val m = mark()
    pModuleDecl.run()
    m.done(E.MODULE)
    true
  }

  private def pModuleDecl = Parse {
    if (getTokenType != T.MODULETOKEN) {
      false
    } else {
      val m = mark()
      advanceLexer() // skip seen MODULETOKEN
      pModuleName.run()
      pModuleExports.run()
      if (getTokenType == T.WHERE) advanceLexer()
      m.done(E.MODULE_DECL)
      true
    }
  }

  private def pModuleName = Parse {
    val m = mark()
    if (pQConid.run()) {
      m.done(E.MODULE_NAME)
      true
    } else {
      m.rollbackTo()
      false
    }
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
      val m = mark()
      advanceLexer() // skip seen LPAREN
      while (getTokenType != T.RPAREN) {
        // skip commas
        while (getTokenType == T.COMMA) advanceLexer()
        pModuleExport.run()
        // skip commas
        while (getTokenType == T.COMMA) advanceLexer()
      }
      m.done(E.MODULE_EXPORTS)
      true
    }
  }

  private def pModuleExport = {
    pModuleExportModule
      .orElse(pModuleExportTyCon)
  }

  private def pModuleExportModule = Parse {
    if (getTokenType != T.MODULETOKEN) {
      false
    } else {
      val m = mark()
      advanceLexer() // skip seen MODULETOKEN
      if (!pModuleName.run()) error("Missing module name")
      m.done(E.MODULE_EXPORT_MODULE)
      true
    }
  }

  private def pModuleExportTyCon = Parse {
    val m = mark()
    if (!pQTyCon.run()) {
      m.rollbackTo()
      false
    } else {
      m.done(E.MODULE_EXPORT_TYCON)
      true
    }
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
      while (getTokenType == T.CONIDREGEXP && lookAhead(1) == T.PERIOD) {
        assert(pConid.run())
        assert(getTokenType == T.PERIOD)
        advanceLexer()
      }
      if (p.run()) {
        m.done(e)
      } else {
        m.rollbackTo()
      }
    }.isDone
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
      def getExports: List[ModuleExport]
    }

    sealed trait ModuleExport extends HElement

    trait ModuleExportModule extends ModuleExport {
      def getModuleName: ModuleName
    }

    trait ModuleExportTyCon extends ModuleExport {
      def getQTyCon: QTyCon
      def getDoublePeriod: Option[PsiElement]
      final def exportsAllMembers: Boolean = getDoublePeriod.isDefined
      def getExportedMembers: List[QVar]
    }

    trait ModuleExportVar extends ModuleExport {
      def getQVar: QVar
    }

    trait QualifiedPrefix extends HElement {
      def getConids: List[Conid]
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
}
