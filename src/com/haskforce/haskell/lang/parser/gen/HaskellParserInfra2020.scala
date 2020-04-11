////////////////////////////////////////////
// THIS IS A GENERATED FILE; DO NOT EDIT! //
////////////////////////////////////////////

package com.haskforce.haskell.lang.parser.gen

import com.haskforce.haskell.lang.parser.{HaskellTokenTypes2020 => T}
import com.haskforce.HaskellLanguage
import com.haskforce.psi.HaskellTokenType
import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.IElementType

object Elements {

  sealed class HElementType(name: String) extends IElementType(name, HaskellLanguage.INSTANCE)

  object IMPORT_HIDDENS extends HElementType("IMPORT_HIDDENS")

  object IMPORT_HIDDEN extends HElementType("IMPORT_HIDDEN")

  object MODULE_EXPORTS extends HElementType("MODULE_EXPORTS")

  object VARSYM extends HElementType("VARSYM")
  object VARID extends HElementType("VARID")

  object IMPORT_TYPE_CONID extends HElementType("IMPORT_TYPE_CONID")
  object IMPORT_TYPE_CONSYM extends HElementType("IMPORT_TYPE_CONSYM")

  object MODULE_NAME extends HElementType("MODULE_NAME")

  object IMPORT_MEMBER_VARSYM extends HElementType("IMPORT_MEMBER_VARSYM")
  object IMPORT_MEMBER_CONID extends HElementType("IMPORT_MEMBER_CONID")
  object IMPORT_MEMBER_VARID extends HElementType("IMPORT_MEMBER_VARID")
  object IMPORT_MEMBER_CONSYM extends HElementType("IMPORT_MEMBER_CONSYM")

  object UNKNOWN extends HElementType("UNKNOWN")

  object IMPORT_EXPLICITS extends HElementType("IMPORT_EXPLICITS")

  object IMPORT_STMT extends HElementType("IMPORT_STMT")

  object IMPORT_EXPLICIT extends HElementType("IMPORT_EXPLICIT")

  object QUALIFIED_PREFIX extends HElementType("QUALIFIED_PREFIX")

  object MODULE_EXPORT_MODULE extends HElementType("MODULE_EXPORT_MODULE")
  object MODULE_EXPORT_TYCON extends HElementType("MODULE_EXPORT_TYCON")
  object MODULE_EXPORT_VAR extends HElementType("MODULE_EXPORT_VAR")

  object QVAR extends HElementType("QVAR")

  object MODULE_DECL extends HElementType("MODULE_DECL")

  object QCONSYM extends HElementType("QCONSYM")

  object TYCON_CONSYM extends HElementType("TYCON_CONSYM")
  object TYCON_CONID extends HElementType("TYCON_CONID")

  object CONID extends HElementType("CONID")

  object MODULE extends HElementType("MODULE")

  object CONSYM extends HElementType("CONSYM")

  object IMPORT_ALIAS extends HElementType("IMPORT_ALIAS")

  object IMPORT_MEMBERS extends HElementType("IMPORT_MEMBERS")

  object QCONID extends HElementType("QCONID")

  object QTYCON extends HElementType("QTYCON")
}

object Psi {

  trait HElement extends PsiElement

  type HTokenElement[A] = PsiElement

  trait ImportHiddens extends HElement {
    def getHiddens: util.List[ImportHidden]
  }

  trait ImportHidden extends HElement {
    def getImportType: Option[ImportType]
  }

  trait ModuleExports extends HElement {
    def getExports: util.List[ModuleExport]
  }

  trait Var extends HElement {}

  trait Varsym extends Var {
    def getVarsymTok: HTokenElement[T.VARSYMTOK.type]
  }

  trait Varid extends Var {
    def getVaridRegexp: HTokenElement[T.VARIDREGEXP.type]
  }

  trait ImportType extends HElement {
    def getMembers: Option[ImportMembers]
  }

  trait ImportTypeConid extends ImportType {
    def getConid: Conid
  }

  trait ImportTypeConsym extends ImportType {
    def getConsym: Consym
  }

  trait ModuleName extends HElement {
    def getQconid: Qconid
  }

  trait ImportMember extends HElement {}

  trait ImportMemberVarsym extends ImportMember {
    def getVarsym: Varsym
  }

  trait ImportMemberConid extends ImportMember {
    def getConid: Conid
  }

  trait ImportMemberVarid extends ImportMember {
    def getVarid: Varid
  }

  trait ImportMemberConsym extends ImportMember {
    def getConsym: Consym
  }

  trait Unknown extends HElement {}

  trait ImportExplicits extends HElement {
    def getExplicits: util.List[ImportExplicit]
  }

  trait ImportStmt extends HElement {
    def getAlias: Option[ImportAlias]
    def getHiddens: Option[ImportHiddens]
    def getExplicits: Option[ImportExplicits]
  }

  trait ImportExplicit extends HElement {}

  trait QualifiedPrefix extends HElement {
    def getConids: util.List[Conid]
  }

  trait ModuleExport extends HElement {}

  trait ModuleExportModule extends ModuleExport {
    def getModuleName: ModuleName
  }

  trait ModuleExportTycon extends ModuleExport {
    final def exportsAllMembers: Boolean = getDoublePeriod.isDefined
    def getDoublePeriod: Option[HTokenElement[T.DOUBLEPERIOD.type]]
    def getQtycon: Qtycon
    def getExportedMembers: util.List[Qvar]
  }

  trait ModuleExportVar extends ModuleExport {
    def getQvar: Qvar
  }

  trait Qvar extends HElement {
    def getQualifiedPrefix: Option[QualifiedPrefix]
    def getVar: Var
  }

  trait ModuleDecl extends HElement {
    def getModuleName: Option[ModuleName]
  }

  trait Qconsym extends HElement {
    def getQualifiedPrefix: Option[QualifiedPrefix]
    def getConsym: Consym
  }

  trait Tycon extends HElement {}

  trait TyconConsym extends Tycon {
    def getConsym: Consym
  }

  trait TyconConid extends Tycon {
    def getConid: Conid
  }

  trait Conid extends HElement {
    def getConidRegexp: HTokenElement[T.CONIDREGEXP.type]
  }

  trait Module extends HElement {
    def getModuleDecl: Option[ModuleDecl]
  }

  trait Consym extends HElement {
    def getConsymTok: HTokenElement[T.CONSYMTOK.type]
  }

  trait ImportAlias extends HElement {
    def getQconid: Qconid
  }

  trait ImportMembers extends HElement {
    def getMembers: util.List[ImportMember]
  }

  trait Qconid extends HElement {
    def getQualifiedPrefix: Option[QualifiedPrefix]
    def getConid: Conid
  }

  trait Qtycon extends HElement {
    def getQualifiedPrefix: Option[QualifiedPrefix]
    def getTycon: Tycon
  }
}

object PsiImpl {
    abstract class HElementImpl(node: ASTNode) extends ASTWrapperPsiElement(node) with Psi.HElement {
      override def toString: String = node.getElementType.toString

      protected def one[A <: Psi.HElement](implicit ct: ClassTag[A]): A = {
        notNullChild(PsiTreeUtil.getChildOfType[A](this, cls[A]))
      }

      protected def option[A <: Psi.HElement](implicit ct: ClassTag[A]): Option[A] = {
        Option(PsiTreeUtil.getChildOfType[A](this, cls[A]))
      }

      protected def list[A <: Psi.HElement](implicit ct: ClassTag[A]): util.List[A] = {
        PsiTreeUtil.getChildrenOfTypeAsList[A](this, cls[A])
      }

      protected def oneTok(t: HaskellTokenType): PsiElement = {
        notNullChild(findChildByType[PsiElement](t))
      }

      //noinspection SameParameterValue
      protected def optionTok(t: HaskellTokenType): Option[PsiElement] = {
        Option(findChildByType[PsiElement](t))
      }
    }

    private def cls[A](implicit ct: ClassTag[A]): Class[A] = {
      ct.runtimeClass.asInstanceOf[Class[A]]
    }

  class ImportHiddensImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportHiddens {
    override def getHiddens: util.List[ImportHidden] = list
  }

  class ImportHiddenImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportHidden {
    override def getImportType: Option[ImportType] = option
  }

  class ModuleExportsImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExports {
    override def getExports: util.List[ModuleExport] = list
  }

  class VarsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.Var {
    override def getVarsymTok: HTokenElement[T.VARSYMTOK.type] = oneTok(T.VARSYMTOK)
  }

  class VaridImpl(node: ASTNode) extends HElementImpl(node) with Psi.Var {
    override def getVaridRegexp: HTokenElement[T.VARIDREGEXP.type] = oneTok(T.VARIDREGEXP)
  }

  class ImportTypeConidImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportType {
    override def getConid: Conid = one
  }

  class ImportTypeConsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportType {
    override def getConsym: Consym = one
  }

  class ModuleNameImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleName {
    override def getQconid: Qconid = one
  }

  class ImportMemberVarsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportMember {
    override def getVarsym: Varsym = one
  }

  class ImportMemberConidImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportMember {
    override def getConid: Conid = one
  }

  class ImportMemberVaridImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportMember {
    override def getVarid: Varid = one
  }

  class ImportMemberConsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportMember {
    override def getConsym: Consym = one
  }

  class UnknownImpl(node: ASTNode) extends HElementImpl(node) with Psi.Unknown {}

  class ImportExplicitsImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportExplicits {
    override def getExplicits: util.List[ImportExplicit] = list
  }

  class ImportStmtImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportStmt {
    override def getAlias: Option[ImportAlias] = option
    override def getHiddens: Option[ImportHiddens] = option
    override def getExplicits: Option[ImportExplicits] = option
  }

  class ImportExplicitImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportExplicit {}

  class QualifiedPrefixImpl(node: ASTNode) extends HElementImpl(node) with Psi.QualifiedPrefix {
    override def getConids: util.List[Conid] = list
  }

  class ModuleExportModuleImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExport {
    override def getModuleName: ModuleName = one
  }

  class ModuleExportTyconImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExport {
    override def getDoublePeriod: Option[HTokenElement[T.DOUBLEPERIOD.type]] = optionTok(T.DOUBLEPERIOD)
    override def getQtycon: Qtycon = one
    override def getExportedMembers: util.List[Qvar] = list
  }

  class ModuleExportVarImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleExport {
    override def getQvar: Qvar = one
  }

  class QvarImpl(node: ASTNode) extends HElementImpl(node) with Psi.Qvar {
    override def getQualifiedPrefix: Option[QualifiedPrefix] = option
    override def getVar: Var = one
  }

  class ModuleDeclImpl(node: ASTNode) extends HElementImpl(node) with Psi.ModuleDecl {
    override def getModuleName: Option[ModuleName] = option
  }

  class QconsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.Qconsym {
    override def getQualifiedPrefix: Option[QualifiedPrefix] = option
    override def getConsym: Consym = one
  }

  class TyconConsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.Tycon {
    override def getConsym: Consym = one
  }

  class TyconConidImpl(node: ASTNode) extends HElementImpl(node) with Psi.Tycon {
    override def getConid: Conid = one
  }

  class ConidImpl(node: ASTNode) extends HElementImpl(node) with Psi.Conid {
    override def getConidRegexp: HTokenElement[T.CONIDREGEXP.type] = oneTok(T.CONIDREGEXP)
  }

  class ModuleImpl(node: ASTNode) extends HElementImpl(node) with Psi.Module {
    override def getModuleDecl: Option[ModuleDecl] = option
  }

  class ConsymImpl(node: ASTNode) extends HElementImpl(node) with Psi.Consym {
    override def getConsymTok: HTokenElement[T.CONSYMTOK.type] = oneTok(T.CONSYMTOK)
  }

  class ImportAliasImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportAlias {
    override def getQconid: Qconid = one
  }

  class ImportMembersImpl(node: ASTNode) extends HElementImpl(node) with Psi.ImportMembers {
    override def getMembers: util.List[ImportMember] = list
  }

  class QconidImpl(node: ASTNode) extends HElementImpl(node) with Psi.Qconid {
    override def getQualifiedPrefix: Option[QualifiedPrefix] = option
    override def getConid: Conid = one
  }

  class QtyconImpl(node: ASTNode) extends HElementImpl(node) with Psi.Qtycon {
    override def getQualifiedPrefix: Option[QualifiedPrefix] = option
    override def getTycon: Tycon = one
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
      case Elements.IMPORT_HIDDENS => new PsiImpl.ImportHiddensImpl(node)
      case Elements.IMPORT_HIDDEN => new PsiImpl.ImportHiddenImpl(node)
      case Elements.MODULE_EXPORTS => new PsiImpl.ModuleExportsImpl(node)
      case Elements.MODULE_NAME => new PsiImpl.ModuleNameImpl(node)
      case Elements.UNKNOWN => new PsiImpl.UnknownImpl(node)
      case Elements.IMPORT_EXPLICITS => new PsiImpl.ImportExplicitsImpl(node)
      case Elements.IMPORT_STMT => new PsiImpl.ImportStmtImpl(node)
      case Elements.IMPORT_EXPLICIT => new PsiImpl.ImportExplicitImpl(node)
      case Elements.QUALIFIED_PREFIX => new PsiImpl.QualifiedPrefixImpl(node)
      case Elements.QVAR => new PsiImpl.QvarImpl(node)
      case Elements.MODULE_DECL => new PsiImpl.ModuleDeclImpl(node)
      case Elements.QCONSYM => new PsiImpl.QconsymImpl(node)
      case Elements.CONID => new PsiImpl.ConidImpl(node)
      case Elements.MODULE => new PsiImpl.ModuleImpl(node)
      case Elements.CONSYM => new PsiImpl.ConsymImpl(node)
      case Elements.IMPORT_ALIAS => new PsiImpl.ImportAliasImpl(node)
      case Elements.IMPORT_MEMBERS => new PsiImpl.ImportMembersImpl(node)
      case Elements.QCONID => new PsiImpl.QconidImpl(node)
      case Elements.QTYCON => new PsiImpl.QtyconImpl(node)
    }
  }
