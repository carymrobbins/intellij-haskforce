// This is a generated file. Not intended for manual editing.
package com.haskforce.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.openapi.diagnostic.Logger;
import static com.haskforce.psi.HaskellTypes.*;
import static org.intellij.grammar.parser.GeneratedParserUtilBase.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class HaskellParser implements PsiParser {

  public static final Logger LOG_ = Logger.getInstance("com.haskforce.parser.HaskellParser");

  public ASTNode parse(IElementType root_, PsiBuilder builder_) {
    boolean result_;
    builder_ = adapt_builder_(root_, builder_, this, null);
    Marker marker_ = enter_section_(builder_, 0, _COLLAPSE_, null);
    if (root_ == ANYSEQ) {
      result_ = anyseq(builder_, 0);
    }
    else if (root_ == ATYPE) {
      result_ = atype(builder_, 0);
    }
    else if (root_ == BODY) {
      result_ = body(builder_, 0);
    }
    else if (root_ == CDECL) {
      result_ = cdecl(builder_, 0);
    }
    else if (root_ == CLASSS) {
      result_ = classs(builder_, 0);
    }
    else if (root_ == CNAME) {
      result_ = cname(builder_, 0);
    }
    else if (root_ == CNAMES) {
      result_ = cnames(builder_, 0);
    }
    else if (root_ == CON) {
      result_ = con(builder_, 0);
    }
    else if (root_ == CONOP) {
      result_ = conop(builder_, 0);
    }
    else if (root_ == CONSTR) {
      result_ = constr(builder_, 0);
    }
    else if (root_ == CONSTRS) {
      result_ = constrs(builder_, 0);
    }
    else if (root_ == CONSYM) {
      result_ = consym(builder_, 0);
    }
    else if (root_ == CONTEXT) {
      result_ = context(builder_, 0);
    }
    else if (root_ == EXPORT) {
      result_ = export(builder_, 0);
    }
    else if (root_ == EXPORTS) {
      result_ = exports(builder_, 0);
    }
    else if (root_ == FDECL) {
      result_ = fdecl(builder_, 0);
    }
    else if (root_ == FIXITY) {
      result_ = fixity(builder_, 0);
    }
    else if (root_ == FUNLHS) {
      result_ = funlhs(builder_, 0);
    }
    else if (root_ == GCONSYM) {
      result_ = gconsym(builder_, 0);
    }
    else if (root_ == GENDECL) {
      result_ = gendecl(builder_, 0);
    }
    else if (root_ == IDECL) {
      result_ = idecl(builder_, 0);
    }
    else if (root_ == IMPDECL) {
      result_ = impdecl(builder_, 0);
    }
    else if (root_ == IMPORTT) {
      result_ = importt(builder_, 0);
    }
    else if (root_ == INST) {
      result_ = inst(builder_, 0);
    }
    else if (root_ == MODULE_PREFIX) {
      result_ = modulePrefix(builder_, 0);
    }
    else if (root_ == NCOMMENT) {
      result_ = ncomment(builder_, 0);
    }
    else if (root_ == NEWCONSTR) {
      result_ = newconstr(builder_, 0);
    }
    else if (root_ == OP) {
      result_ = op(builder_, 0);
    }
    else if (root_ == OPS) {
      result_ = ops(builder_, 0);
    }
    else if (root_ == PRAGMA) {
      result_ = pragma(builder_, 0);
    }
    else if (root_ == QCON) {
      result_ = qcon(builder_, 0);
    }
    else if (root_ == QCONID) {
      result_ = qconid(builder_, 0);
    }
    else if (root_ == QCONOP) {
      result_ = qconop(builder_, 0);
    }
    else if (root_ == QCONSYM) {
      result_ = qconsym(builder_, 0);
    }
    else if (root_ == QINFIXCONID) {
      result_ = qinfixconid(builder_, 0);
    }
    else if (root_ == QINFIXVARID) {
      result_ = qinfixvarid(builder_, 0);
    }
    else if (root_ == QOP) {
      result_ = qop(builder_, 0);
    }
    else if (root_ == QTYCLS) {
      result_ = qtycls(builder_, 0);
    }
    else if (root_ == QTYCON) {
      result_ = qtycon(builder_, 0);
    }
    else if (root_ == QVAR) {
      result_ = qvar(builder_, 0);
    }
    else if (root_ == QVARID) {
      result_ = qvarid(builder_, 0);
    }
    else if (root_ == QVAROP) {
      result_ = qvarop(builder_, 0);
    }
    else if (root_ == QVARS) {
      result_ = qvars(builder_, 0);
    }
    else if (root_ == QVARSYM) {
      result_ = qvarsym(builder_, 0);
    }
    else if (root_ == RESERVEDOP) {
      result_ = reservedop(builder_, 0);
    }
    else if (root_ == RHS) {
      result_ = rhs(builder_, 0);
    }
    else if (root_ == SCONTEXT) {
      result_ = scontext(builder_, 0);
    }
    else if (root_ == SPECIAL) {
      result_ = special(builder_, 0);
    }
    else if (root_ == SYMBOL) {
      result_ = symbol(builder_, 0);
    }
    else if (root_ == TYCLS) {
      result_ = tycls(builder_, 0);
    }
    else if (root_ == TYCON) {
      result_ = tycon(builder_, 0);
    }
    else if (root_ == TYPEE) {
      result_ = typee(builder_, 0);
    }
    else if (root_ == TYVAR) {
      result_ = tyvar(builder_, 0);
    }
    else if (root_ == VAR) {
      result_ = var(builder_, 0);
    }
    else if (root_ == VAROP) {
      result_ = varop(builder_, 0);
    }
    else if (root_ == VARS) {
      result_ = vars(builder_, 0);
    }
    else if (root_ == VARSYM) {
      result_ = varsym(builder_, 0);
    }
    else if (root_ == WHITECHAR) {
      result_ = whitechar(builder_, 0);
    }
    else {
      result_ = parse_root_(root_, builder_, 0);
    }
    exit_section_(builder_, 0, marker_, root_, result_, true, TRUE_CONDITION);
    return builder_.getTreeBuilt();
  }

  protected boolean parse_root_(final IElementType root_, final PsiBuilder builder_, final int level_) {
    return module(builder_, level_ + 1);
  }

  /* ********************************************************** */
  // !( opencom | closecom ) (lexeme | whitechar)+
  public static boolean anyseq(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anyseq")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<anyseq>");
    result_ = anyseq_0(builder_, level_ + 1);
    result_ = result_ && anyseq_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ANYSEQ, result_, false, null);
    return result_;
  }

  // !( opencom | closecom )
  private static boolean anyseq_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anyseq_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !anyseq_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // opencom | closecom
  private static boolean anyseq_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anyseq_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OPENCOM);
    if (!result_) result_ = consumeToken(builder_, CLOSECOM);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (lexeme | whitechar)+
  private static boolean anyseq_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anyseq_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = anyseq_1_0(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!anyseq_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "anyseq_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // lexeme | whitechar
  private static boolean anyseq_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anyseq_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = lexeme(builder_, level_ + 1);
    if (!result_) result_ = whitechar(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // gtycon
  //         | tyvar
  //         | <<commaSeparate typee>>
  //         | '[' typee ']'
  //         | '(' typee ')'
  public static boolean atype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<atype>");
    result_ = gtycon(builder_, level_ + 1);
    if (!result_) result_ = tyvar(builder_, level_ + 1);
    if (!result_) result_ = commaSeparate(builder_, level_ + 1, typee_parser_);
    if (!result_) result_ = atype_3(builder_, level_ + 1);
    if (!result_) result_ = atype_4(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ATYPE, result_, false, null);
    return result_;
  }

  // '[' typee ']'
  private static boolean atype_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && typee(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACKET);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' typee ')'
  private static boolean atype_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && typee(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // impdecl* topdecl*
  public static boolean body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "body")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<body>");
    result_ = body_0(builder_, level_ + 1);
    result_ = result_ && body_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, BODY, result_, false, null);
    return result_;
  }

  // impdecl*
  private static boolean body_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "body_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!impdecl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "body_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // topdecl*
  private static boolean body_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "body_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!topdecl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "body_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // atype btype
  //                 | atype
  static boolean btype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "btype")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = btype_0(builder_, level_ + 1);
    if (!result_) result_ = atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // atype btype
  private static boolean btype_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "btype_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atype(builder_, level_ + 1);
    result_ = result_ && btype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // gendecl
  //              | (funlhs | var) rhs
  public static boolean cdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _LEFT_, "<cdecl>");
    result_ = gendecl(builder_, level_ + 1);
    if (!result_) result_ = cdecl_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CDECL, result_, false, null);
    return result_;
  }

  // (funlhs | var) rhs
  private static boolean cdecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecl_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = cdecl_1_0(builder_, level_ + 1);
    result_ = result_ && rhs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // funlhs | var
  private static boolean cdecl_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecl_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = funlhs(builder_, level_ + 1);
    if (!result_) result_ = var(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // qtycls tyvar
  //         | qtycls '(' tyvar atype+ ')'
  public static boolean classs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "classs")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = classs_0(builder_, level_ + 1);
    if (!result_) result_ = classs_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, CLASSS, result_);
    return result_;
  }

  // qtycls tyvar
  private static boolean classs_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "classs_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycls(builder_, level_ + 1);
    result_ = result_ && tyvar(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qtycls '(' tyvar atype+ ')'
  private static boolean classs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "classs_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycls(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LPAREN);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && classs_1_3(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // atype+
  private static boolean classs_1_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "classs_1_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atype(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!atype(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "classs_1_3", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // var | con
  public static boolean cname(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cname")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<cname>");
    result_ = var(builder_, level_ + 1);
    if (!result_) result_ = con(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CNAME, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // <<commaSeparate cname>>
  public static boolean cnames(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cnames")) return false;
    if (!nextTokenIs(builder_, LPAREN)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = commaSeparate(builder_, level_ + 1, cname_parser_);
    exit_section_(builder_, marker_, CNAMES, result_);
    return result_;
  }

  /* ********************************************************** */
  // '(' <<p>> (',' <<p>>)* ')'
  static boolean commaSeparate(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "commaSeparate")) return false;
    if (!nextTokenIs(builder_, LPAREN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, LPAREN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, p.parse(builder_, level_));
    result_ = pinned_ && report_error_(builder_, commaSeparate_2(builder_, level_ + 1, p)) && result_;
    result_ = pinned_ && consumeToken(builder_, RPAREN) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' <<p>>)*
  private static boolean commaSeparate_2(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "commaSeparate_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!commaSeparate_2_0(builder_, level_ + 1, p)) break;
      if (!empty_element_parsed_guard_(builder_, "commaSeparate_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ',' <<p>>
  private static boolean commaSeparate_2_0(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "commaSeparate_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && p.parse(builder_, level_);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '(' <<p>> (',' (<<p>> | &')'))* ')'
  static boolean commaSeparate2(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "commaSeparate2")) return false;
    if (!nextTokenIs(builder_, LPAREN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, LPAREN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, p.parse(builder_, level_));
    result_ = pinned_ && report_error_(builder_, commaSeparate2_2(builder_, level_ + 1, p)) && result_;
    result_ = pinned_ && consumeToken(builder_, RPAREN) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' (<<p>> | &')'))*
  private static boolean commaSeparate2_2(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "commaSeparate2_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!commaSeparate2_2_0(builder_, level_ + 1, p)) break;
      if (!empty_element_parsed_guard_(builder_, "commaSeparate2_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ',' (<<p>> | &')')
  private static boolean commaSeparate2_2_0(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "commaSeparate2_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && commaSeparate2_2_0_1(builder_, level_ + 1, p);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // <<p>> | &')'
  private static boolean commaSeparate2_2_0_1(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "commaSeparate2_2_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = p.parse(builder_, level_);
    if (!result_) result_ = commaSeparate2_2_0_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // &')'
  private static boolean commaSeparate2_2_0_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "commaSeparate2_2_0_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = consumeToken(builder_, RPAREN);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // conid | '(' consym ')'
  public static boolean con(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "con")) return false;
    if (!nextTokenIs(builder_, "<con>", LPAREN, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<con>");
    result_ = consumeToken(builder_, CONID);
    if (!result_) result_ = con_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CON, result_, false, null);
    return result_;
  }

  // '(' consym ')'
  private static boolean con_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "con_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && consym(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // consym | '`' conid '`'
  public static boolean conop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "conop")) return false;
    if (!nextTokenIs(builder_, "<conop>", COLON, BACKTICK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<conop>");
    result_ = consym(builder_, level_ + 1);
    if (!result_) result_ = conop_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CONOP, result_, false, null);
    return result_;
  }

  // '`' conid '`'
  private static boolean conop_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "conop_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && consumeToken(builder_, CONID);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // con (['!'] atype)*
  //         | (btype | '!'atype) conop (btype | '!'atype)
  //         | con '{' ((fielddecl ',')* fielddecl)? '}'
  public static boolean constr(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<constr>");
    result_ = constr_0(builder_, level_ + 1);
    if (!result_) result_ = constr_1(builder_, level_ + 1);
    if (!result_) result_ = constr_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CONSTR, result_, false, null);
    return result_;
  }

  // con (['!'] atype)*
  private static boolean constr_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = con(builder_, level_ + 1);
    result_ = result_ && constr_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (['!'] atype)*
  private static boolean constr_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!constr_0_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "constr_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ['!'] atype
  private static boolean constr_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = constr_0_1_0_0(builder_, level_ + 1);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['!']
  private static boolean constr_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0_1_0_0")) return false;
    consumeToken(builder_, EXLAMATION);
    return true;
  }

  // (btype | '!'atype) conop (btype | '!'atype)
  private static boolean constr_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = constr_1_0(builder_, level_ + 1);
    result_ = result_ && conop(builder_, level_ + 1);
    result_ = result_ && constr_1_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // btype | '!'atype
  private static boolean constr_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = btype(builder_, level_ + 1);
    if (!result_) result_ = constr_1_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '!'atype
  private static boolean constr_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EXLAMATION);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // btype | '!'atype
  private static boolean constr_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = btype(builder_, level_ + 1);
    if (!result_) result_ = constr_1_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '!'atype
  private static boolean constr_1_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1_2_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EXLAMATION);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // con '{' ((fielddecl ',')* fielddecl)? '}'
  private static boolean constr_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = con(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LBRACE);
    result_ = result_ && constr_2_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ((fielddecl ',')* fielddecl)?
  private static boolean constr_2_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_2")) return false;
    constr_2_2_0(builder_, level_ + 1);
    return true;
  }

  // (fielddecl ',')* fielddecl
  private static boolean constr_2_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = constr_2_2_0_0(builder_, level_ + 1);
    result_ = result_ && fielddecl(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (fielddecl ',')*
  private static boolean constr_2_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_2_0_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!constr_2_2_0_0_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "constr_2_2_0_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // fielddecl ','
  private static boolean constr_2_2_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_2_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fielddecl(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // constr ('|' constr)*
  public static boolean constrs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constrs")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<constrs>");
    result_ = constr(builder_, level_ + 1);
    result_ = result_ && constrs_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CONSTRS, result_, false, null);
    return result_;
  }

  // ('|' constr)*
  private static boolean constrs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constrs_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!constrs_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "constrs_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // '|' constr
  private static boolean constrs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constrs_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, PIPE);
    result_ = result_ && constr(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !reservedop ( ':' symbol* )
  public static boolean consym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym")) return false;
    if (!nextTokenIs(builder_, COLON)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consym_0(builder_, level_ + 1);
    result_ = result_ && consym_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, CONSYM, result_);
    return result_;
  }

  // !reservedop
  private static boolean consym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !reservedop(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // ':' symbol*
  private static boolean consym_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COLON);
    result_ = result_ && consym_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // symbol*
  private static boolean consym_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym_1_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!symbol(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "consym_1_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // classs
  //           | <<commaSeparate classs>>
  public static boolean context(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "context")) return false;
    if (!nextTokenIs(builder_, "<context>", LPAREN, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<context>");
    result_ = classs(builder_, level_ + 1);
    if (!result_) result_ = commaSeparate(builder_, level_ + 1, classs_parser_);
    exit_section_(builder_, level_, marker_, CONTEXT, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // qtycls
  static boolean dclass(PsiBuilder builder_, int level_) {
    return qtycls(builder_, level_ + 1);
  }

  /* ********************************************************** */
  // whitespace | lexeme
  static boolean decl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitespace(builder_, level_ + 1);
    if (!result_) result_ = lexeme(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "deriving" (dclass|<<commaSeparate dclass>>)
  static boolean deriving(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "deriving")) return false;
    if (!nextTokenIs(builder_, DERIVING)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DERIVING);
    result_ = result_ && deriving_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // dclass|<<commaSeparate dclass>>
  private static boolean deriving_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "deriving_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = dclass(builder_, level_ + 1);
    if (!result_) result_ = commaSeparate(builder_, level_ + 1, dclass_parser_);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // qvar
  //         | qtycon ["(..)" | cnames]
  //         | qtycls ["(..)" | qvars]
  //         | "module" qconid
  public static boolean export(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<export>");
    result_ = qvar(builder_, level_ + 1);
    if (!result_) result_ = export_1(builder_, level_ + 1);
    if (!result_) result_ = export_2(builder_, level_ + 1);
    if (!result_) result_ = export_3(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, EXPORT, result_, false, null);
    return result_;
  }

  // qtycon ["(..)" | cnames]
  private static boolean export_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycon(builder_, level_ + 1);
    result_ = result_ && export_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["(..)" | cnames]
  private static boolean export_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_1_1")) return false;
    export_1_1_0(builder_, level_ + 1);
    return true;
  }

  // "(..)" | cnames
  private static boolean export_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "(..)");
    if (!result_) result_ = cnames(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qtycls ["(..)" | qvars]
  private static boolean export_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycls(builder_, level_ + 1);
    result_ = result_ && export_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["(..)" | qvars]
  private static boolean export_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_2_1")) return false;
    export_2_1_0(builder_, level_ + 1);
    return true;
  }

  // "(..)" | qvars
  private static boolean export_2_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_2_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "(..)");
    if (!result_) result_ = qvars(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "module" qconid
  private static boolean export_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, MODULE);
    result_ = result_ && qconid(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // <<commaSeparate2 export>>
  public static boolean exports(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exports")) return false;
    if (!nextTokenIs(builder_, LPAREN)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = commaSeparate2(builder_, level_ + 1, export_parser_);
    exit_section_(builder_, marker_, EXPORTS, result_);
    return result_;
  }

  /* ********************************************************** */
  // decl+
  public static boolean fdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<fdecl>");
    result_ = decl(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!decl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "fdecl", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, level_, marker_, FDECL, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // vars '::' (typee | '!' atype)
  static boolean fielddecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fielddecl")) return false;
    if (!nextTokenIs(builder_, "", LPAREN, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = vars(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && fielddecl_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // typee | '!' atype
  private static boolean fielddecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fielddecl_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = typee(builder_, level_ + 1);
    if (!result_) result_ = fielddecl_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '!' atype
  private static boolean fielddecl_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fielddecl_2_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EXLAMATION);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "infix" | "infixr" | "infixl"
  public static boolean fixity(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fixity")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<fixity>");
    result_ = consumeToken(builder_, INFIX);
    if (!result_) result_ = consumeToken(builder_, INFIXR);
    if (!result_) result_ = consumeToken(builder_, INFIXL);
    exit_section_(builder_, level_, marker_, FIXITY, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // decl+
  public static boolean funlhs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funlhs")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<funlhs>");
    result_ = decl(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!decl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "funlhs", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, level_, marker_, FUNLHS, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // ':' | qconsym
  public static boolean gconsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gconsym")) return false;
    if (!nextTokenIs(builder_, "<gconsym>", COLON, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<gconsym>");
    result_ = consumeToken(builder_, COLON);
    if (!result_) result_ = qconsym(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, GCONSYM, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // vars '::' [context '=>'] typee
  //          | fixity [integertoken] ops
  public static boolean gendecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<gendecl>");
    result_ = gendecl_0(builder_, level_ + 1);
    if (!result_) result_ = gendecl_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, GENDECL, result_, false, null);
    return result_;
  }

  // vars '::' [context '=>'] typee
  private static boolean gendecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = vars(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && gendecl_0_2(builder_, level_ + 1);
    result_ = result_ && typee(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [context '=>']
  private static boolean gendecl_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl_0_2")) return false;
    gendecl_0_2_0(builder_, level_ + 1);
    return true;
  }

  // context '=>'
  private static boolean gendecl_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl_0_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = context(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // fixity [integertoken] ops
  private static boolean gendecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fixity(builder_, level_ + 1);
    result_ = result_ && gendecl_1_1(builder_, level_ + 1);
    result_ = result_ && ops(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [integertoken]
  private static boolean gendecl_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl_1_1")) return false;
    consumeToken(builder_, INTEGERTOKEN);
    return true;
  }

  /* ********************************************************** */
  // qtycon
  //                  | "()"
  //                  | "[]"
  //                  | '(' "->" ')'
  //                  | '(' ',' (',')+ ')'
  static boolean gtycon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gtycon")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycon(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, "()");
    if (!result_) result_ = consumeToken(builder_, "[]");
    if (!result_) result_ = gtycon_3(builder_, level_ + 1);
    if (!result_) result_ = gtycon_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' "->" ')'
  private static boolean gtycon_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gtycon_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && consumeToken(builder_, RIGHTARROW);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' ',' (',')+ ')'
  private static boolean gtycon_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gtycon_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && consumeToken(builder_, COMMA);
    result_ = result_ && gtycon_4_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (',')+
  private static boolean gtycon_4_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gtycon_4_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = gtycon_4_2_0(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!gtycon_4_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "gtycon_4_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (',')
  private static boolean gtycon_4_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gtycon_4_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // (funlhs | var) rhs
  public static boolean idecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<idecl>");
    result_ = idecl_0(builder_, level_ + 1);
    result_ = result_ && rhs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, IDECL, result_, false, null);
    return result_;
  }

  // funlhs | var
  private static boolean idecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = funlhs(builder_, level_ + 1);
    if (!result_) result_ = var(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "import" ["qualified"] qconid ["as" qconid] [impspec]
  public static boolean impdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl")) return false;
    if (!nextTokenIs(builder_, IMPORT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, IMPORT);
    result_ = result_ && impdecl_1(builder_, level_ + 1);
    result_ = result_ && qconid(builder_, level_ + 1);
    result_ = result_ && impdecl_3(builder_, level_ + 1);
    result_ = result_ && impdecl_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, IMPDECL, result_);
    return result_;
  }

  // ["qualified"]
  private static boolean impdecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_1")) return false;
    consumeToken(builder_, QUALIFIED);
    return true;
  }

  // ["as" qconid]
  private static boolean impdecl_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_3")) return false;
    impdecl_3_0(builder_, level_ + 1);
    return true;
  }

  // "as" qconid
  private static boolean impdecl_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_3_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, AS);
    result_ = result_ && qconid(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [impspec]
  private static boolean impdecl_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_4")) return false;
    impspec(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // var
  //           | tycon ["(..)" | cnames]
  //           | tycls ["(..)" | vars]
  public static boolean importt(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importt")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<importt>");
    result_ = var(builder_, level_ + 1);
    if (!result_) result_ = importt_1(builder_, level_ + 1);
    if (!result_) result_ = importt_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, IMPORTT, result_, false, null);
    return result_;
  }

  // tycon ["(..)" | cnames]
  private static boolean importt_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importt_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = tycon(builder_, level_ + 1);
    result_ = result_ && importt_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["(..)" | cnames]
  private static boolean importt_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importt_1_1")) return false;
    importt_1_1_0(builder_, level_ + 1);
    return true;
  }

  // "(..)" | cnames
  private static boolean importt_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importt_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "(..)");
    if (!result_) result_ = cnames(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // tycls ["(..)" | vars]
  private static boolean importt_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importt_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = tycls(builder_, level_ + 1);
    result_ = result_ && importt_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["(..)" | vars]
  private static boolean importt_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importt_2_1")) return false;
    importt_2_1_0(builder_, level_ + 1);
    return true;
  }

  // "(..)" | vars
  private static boolean importt_2_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importt_2_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "(..)");
    if (!result_) result_ = vars(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // <<commaSeparate2 importt>>
  //                   | "hiding" <<commaSeparate2 importt>>
  static boolean impspec(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impspec")) return false;
    if (!nextTokenIs(builder_, "", LPAREN, HIDING)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = commaSeparate2(builder_, level_ + 1, importt_parser_);
    if (!result_) result_ = impspec_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "hiding" <<commaSeparate2 importt>>
  private static boolean impspec_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impspec_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, HIDING);
    result_ = result_ && commaSeparate2(builder_, level_ + 1, importt_parser_);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // gtycon
  //        | '(' gtycon tyvar* ')'
  //        | '(' tyvar ',' (tyvar ',')* tyvar ')'        // No commaSep, >= 2 args.
  //        | '[' tyvar ']'
  //        | '(' tyvar "->" tyvar ')'
  public static boolean inst(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<inst>");
    result_ = gtycon(builder_, level_ + 1);
    if (!result_) result_ = inst_1(builder_, level_ + 1);
    if (!result_) result_ = inst_2(builder_, level_ + 1);
    if (!result_) result_ = inst_3(builder_, level_ + 1);
    if (!result_) result_ = inst_4(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, INST, result_, false, null);
    return result_;
  }

  // '(' gtycon tyvar* ')'
  private static boolean inst_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && gtycon(builder_, level_ + 1);
    result_ = result_ && inst_1_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // tyvar*
  private static boolean inst_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst_1_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!tyvar(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "inst_1_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // '(' tyvar ',' (tyvar ',')* tyvar ')'
  private static boolean inst_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    result_ = result_ && inst_2_3(builder_, level_ + 1);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (tyvar ',')*
  private static boolean inst_2_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst_2_3")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!inst_2_3_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "inst_2_3", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // tyvar ','
  private static boolean inst_2_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst_2_3_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = tyvar(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' tyvar ']'
  private static boolean inst_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACKET);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' tyvar "->" tyvar ')'
  private static boolean inst_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "inst_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RIGHTARROW);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // literal | reservedop | reservedid | pragma
  //                  | qconid | qinfixvarid | qvarid | qinfixconid | qvarsym
  //                  | qconsym | special
  static boolean lexeme(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexeme")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = literal(builder_, level_ + 1);
    if (!result_) result_ = reservedop(builder_, level_ + 1);
    if (!result_) result_ = reservedid(builder_, level_ + 1);
    if (!result_) result_ = pragma(builder_, level_ + 1);
    if (!result_) result_ = qconid(builder_, level_ + 1);
    if (!result_) result_ = qinfixvarid(builder_, level_ + 1);
    if (!result_) result_ = qvarid(builder_, level_ + 1);
    if (!result_) result_ = qinfixconid(builder_, level_ + 1);
    if (!result_) result_ = qvarsym(builder_, level_ + 1);
    if (!result_) result_ = qconsym(builder_, level_ + 1);
    if (!result_) result_ = special(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // floattoken | integertoken | chartoken | stringtoken
  static boolean literal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, FLOATTOKEN);
    if (!result_) result_ = consumeToken(builder_, INTEGERTOKEN);
    if (!result_) result_ = consumeToken(builder_, CHARTOKEN);
    if (!result_) result_ = consumeToken(builder_, STRINGTOKEN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // (whitespace | pragma) module
  //          | "module" qconid [exports] "where" body
  //          | body
  static boolean module(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_0(builder_, level_ + 1);
    if (!result_) result_ = module_1(builder_, level_ + 1);
    if (!result_) result_ = body(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (whitespace | pragma) module
  private static boolean module_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_0_0(builder_, level_ + 1);
    result_ = result_ && module(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // whitespace | pragma
  private static boolean module_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitespace(builder_, level_ + 1);
    if (!result_) result_ = pragma(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "module" qconid [exports] "where" body
  private static boolean module_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, MODULE);
    result_ = result_ && qconid(builder_, level_ + 1);
    result_ = result_ && module_1_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, WHERE);
    result_ = result_ && body(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [exports]
  private static boolean module_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_1_2")) return false;
    exports(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // (conid '.')+
  public static boolean modulePrefix(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modulePrefix")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = modulePrefix_0(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!modulePrefix_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "modulePrefix", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, MODULE_PREFIX, result_);
    return result_;
  }

  // conid '.'
  private static boolean modulePrefix_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modulePrefix_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONID);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // opencom anyseq? (ncomment anyseq?)* closecom
  public static boolean ncomment(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment")) return false;
    if (!nextTokenIs(builder_, OPENCOM)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OPENCOM);
    result_ = result_ && ncomment_1(builder_, level_ + 1);
    result_ = result_ && ncomment_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CLOSECOM);
    exit_section_(builder_, marker_, NCOMMENT, result_);
    return result_;
  }

  // anyseq?
  private static boolean ncomment_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_1")) return false;
    anyseq(builder_, level_ + 1);
    return true;
  }

  // (ncomment anyseq?)*
  private static boolean ncomment_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!ncomment_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "ncomment_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ncomment anyseq?
  private static boolean ncomment_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ncomment(builder_, level_ + 1);
    result_ = result_ && ncomment_2_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // anyseq?
  private static boolean ncomment_2_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_2_0_1")) return false;
    anyseq(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // con atype
  //             | con '{' var '::' typee '}'
  public static boolean newconstr(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newconstr")) return false;
    if (!nextTokenIs(builder_, "<newconstr>", LPAREN, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<newconstr>");
    result_ = newconstr_0(builder_, level_ + 1);
    if (!result_) result_ = newconstr_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, NEWCONSTR, result_, false, null);
    return result_;
  }

  // con atype
  private static boolean newconstr_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newconstr_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = con(builder_, level_ + 1);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // con '{' var '::' typee '}'
  private static boolean newconstr_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newconstr_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = con(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LBRACE);
    result_ = result_ && var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && typee(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // EOL
  static boolean newline(PsiBuilder builder_, int level_) {
    return consumeToken(builder_, EOL);
  }

  /* ********************************************************** */
  // varop | conop
  public static boolean op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "op")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<op>");
    result_ = varop(builder_, level_ + 1);
    if (!result_) result_ = conop(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, OP, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // <<sequence op>>
  public static boolean ops(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ops")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<ops>");
    result_ = sequence(builder_, level_ + 1, op_parser_);
    exit_section_(builder_, level_, marker_, OPS, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // openpragma (!closepragma (lexeme | space))+ closepragma
  public static boolean pragma(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pragma")) return false;
    if (!nextTokenIs(builder_, OPENPRAGMA)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OPENPRAGMA);
    result_ = result_ && pragma_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CLOSEPRAGMA);
    exit_section_(builder_, marker_, PRAGMA, result_);
    return result_;
  }

  // (!closepragma (lexeme | space))+
  private static boolean pragma_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pragma_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pragma_1_0(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!pragma_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "pragma_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !closepragma (lexeme | space)
  private static boolean pragma_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pragma_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pragma_1_0_0(builder_, level_ + 1);
    result_ = result_ && pragma_1_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !closepragma
  private static boolean pragma_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pragma_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !consumeToken(builder_, CLOSEPRAGMA);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // lexeme | space
  private static boolean pragma_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pragma_1_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = lexeme(builder_, level_ + 1);
    if (!result_) result_ = space(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // qconid | '(' gconsym ')'
  public static boolean qcon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qcon")) return false;
    if (!nextTokenIs(builder_, "<qcon>", LPAREN, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qcon>");
    result_ = qconid(builder_, level_ + 1);
    if (!result_) result_ = qcon_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QCON, result_, false, null);
    return result_;
  }

  // '(' gconsym ')'
  private static boolean qcon_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qcon_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && gconsym(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modulePrefix] conid
  public static boolean qconid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconid")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qconid_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CONID);
    exit_section_(builder_, marker_, QCONID, result_);
    return result_;
  }

  // [modulePrefix]
  private static boolean qconid_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconid_0")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // gconsym | '`' qconid '`'
  public static boolean qconop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qconop>");
    result_ = gconsym(builder_, level_ + 1);
    if (!result_) result_ = qconop_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QCONOP, result_, false, null);
    return result_;
  }

  // '`' qconid '`'
  private static boolean qconop_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconop_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && qconid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modulePrefix] consym
  public static boolean qconsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconsym")) return false;
    if (!nextTokenIs(builder_, "<qconsym>", COLON, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qconsym>");
    result_ = qconsym_0(builder_, level_ + 1);
    result_ = result_ && consym(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QCONSYM, result_, false, null);
    return result_;
  }

  // [modulePrefix]
  private static boolean qconsym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconsym_0")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '`' [modulePrefix] conid '`'
  public static boolean qinfixconid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qinfixconid")) return false;
    if (!nextTokenIs(builder_, BACKTICK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && qinfixconid_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CONID);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, QINFIXCONID, result_);
    return result_;
  }

  // [modulePrefix]
  private static boolean qinfixconid_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qinfixconid_1")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '`' [modulePrefix] varid '`'
  public static boolean qinfixvarid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qinfixvarid")) return false;
    if (!nextTokenIs(builder_, BACKTICK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && qinfixvarid_1(builder_, level_ + 1);
    result_ = result_ && varid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, QINFIXVARID, result_);
    return result_;
  }

  // [modulePrefix]
  private static boolean qinfixvarid_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qinfixvarid_1")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // qvarop | qconop
  public static boolean qop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qop>");
    result_ = qvarop(builder_, level_ + 1);
    if (!result_) result_ = qconop(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QOP, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // [modulePrefix] tycls
  public static boolean qtycls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qtycls")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycls_0(builder_, level_ + 1);
    result_ = result_ && tycls(builder_, level_ + 1);
    exit_section_(builder_, marker_, QTYCLS, result_);
    return result_;
  }

  // [modulePrefix]
  private static boolean qtycls_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qtycls_0")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // [modulePrefix] tycon
  public static boolean qtycon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qtycon")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycon_0(builder_, level_ + 1);
    result_ = result_ && tycon(builder_, level_ + 1);
    exit_section_(builder_, marker_, QTYCON, result_);
    return result_;
  }

  // [modulePrefix]
  private static boolean qtycon_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qtycon_0")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // qvarid | '(' qvarsym ')'
  public static boolean qvar(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvar")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qvar>");
    result_ = qvarid(builder_, level_ + 1);
    if (!result_) result_ = qvar_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QVAR, result_, false, null);
    return result_;
  }

  // '(' qvarsym ')'
  private static boolean qvar_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvar_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && qvarsym(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modulePrefix] varid
  public static boolean qvarid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarid")) return false;
    if (!nextTokenIs(builder_, "<qvarid>", CONID, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qvarid>");
    result_ = qvarid_0(builder_, level_ + 1);
    result_ = result_ && varid(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QVARID, result_, false, null);
    return result_;
  }

  // [modulePrefix]
  private static boolean qvarid_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarid_0")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // qvarsym | '`' qvarid '`'
  public static boolean qvarop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qvarop>");
    result_ = qvarsym(builder_, level_ + 1);
    if (!result_) result_ = qvarop_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QVAROP, result_, false, null);
    return result_;
  }

  // '`' qvarid '`'
  private static boolean qvarop_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarop_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && qvarid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // <<commaSeparate qvar>>
  public static boolean qvars(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvars")) return false;
    if (!nextTokenIs(builder_, LPAREN)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = commaSeparate(builder_, level_ + 1, qvar_parser_);
    exit_section_(builder_, marker_, QVARS, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modulePrefix] varsym
  public static boolean qvarsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qvarsym>");
    result_ = qvarsym_0(builder_, level_ + 1);
    result_ = result_ && varsym(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QVARSYM, result_, false, null);
    return result_;
  }

  // [modulePrefix]
  private static boolean qvarsym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarsym_0")) return false;
    modulePrefix(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // 'class' | 'data' | 'default' | 'deriving' | 'foreign' | 'instance'
  //                        | 'newtype' | 'type' | 'where'
  static boolean reservedDecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedDecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CLASSTOKEN);
    if (!result_) result_ = consumeToken(builder_, DATA);
    if (!result_) result_ = consumeToken(builder_, DEFAULT);
    if (!result_) result_ = consumeToken(builder_, DERIVING);
    if (!result_) result_ = consumeToken(builder_, FOREIGN);
    if (!result_) result_ = consumeToken(builder_, INSTANCE);
    if (!result_) result_ = consumeToken(builder_, NEWTYPE);
    if (!result_) result_ = consumeToken(builder_, TYPE);
    if (!result_) result_ = consumeToken(builder_, WHERE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // 'case' | 'do' | 'else' | 'if' | 'in' | 'let' | 'of' | 'then'
  static boolean reservedExpr(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedExpr")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CASE);
    if (!result_) result_ = consumeToken(builder_, DO);
    if (!result_) result_ = consumeToken(builder_, ELSE);
    if (!result_) result_ = consumeToken(builder_, IF);
    if (!result_) result_ = consumeToken(builder_, IN);
    if (!result_) result_ = consumeToken(builder_, LET);
    if (!result_) result_ = consumeToken(builder_, OF);
    if (!result_) result_ = consumeToken(builder_, THEN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // 'as' | 'infix' | 'infixl' | 'infixr'
  static boolean reservedMeta(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedMeta")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, AS);
    if (!result_) result_ = consumeToken(builder_, INFIX);
    if (!result_) result_ = consumeToken(builder_, INFIXL);
    if (!result_) result_ = consumeToken(builder_, INFIXR);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '_'
  static boolean reservedVar(PsiBuilder builder_, int level_) {
    return consumeToken(builder_, "_");
  }

  /* ********************************************************** */
  // reservedExpr | reservedDecl | reservedMeta | reservedVar
  static boolean reservedid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedid")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = reservedExpr(builder_, level_ + 1);
    if (!result_) result_ = reservedDecl(builder_, level_ + 1);
    if (!result_) result_ = reservedMeta(builder_, level_ + 1);
    if (!result_) result_ = reservedVar(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '..' | '::' | '=' | '\' | '|' | '<-' | '->' | '@' | '~' | '=>'
  public static boolean reservedop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<reservedop>");
    result_ = consumeToken(builder_, DOUBLEPERIOD);
    if (!result_) result_ = consumeToken(builder_, DOUBLECOLON);
    if (!result_) result_ = consumeToken(builder_, EQUALS);
    if (!result_) result_ = consumeToken(builder_, BACKSLASH);
    if (!result_) result_ = consumeToken(builder_, PIPE);
    if (!result_) result_ = consumeToken(builder_, LEFTARROW);
    if (!result_) result_ = consumeToken(builder_, RIGHTARROW);
    if (!result_) result_ = consumeToken(builder_, AMPERSAT);
    if (!result_) result_ = consumeToken(builder_, TILDE);
    if (!result_) result_ = consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, level_, marker_, RESERVEDOP, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // decl+
  public static boolean rhs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rhs")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<rhs>");
    result_ = decl(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!decl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "rhs", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, level_, marker_, RHS, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // simpleclass
  //            | <<commaSeparate simpleclass>>
  public static boolean scontext(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "scontext")) return false;
    if (!nextTokenIs(builder_, "<scontext>", LPAREN, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<scontext>");
    result_ = simpleclass(builder_, level_ + 1);
    if (!result_) result_ = commaSeparate(builder_, level_ + 1, simpleclass_parser_);
    exit_section_(builder_, level_, marker_, SCONTEXT, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // <<p>> (',' <<p>>)*
  static boolean sequence(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "sequence")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = p.parse(builder_, level_);
    pinned_ = result_; // pin = 1
    result_ = result_ && sequence_1(builder_, level_ + 1, p);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' <<p>>)*
  private static boolean sequence_1(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "sequence_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!sequence_1_0(builder_, level_ + 1, p)) break;
      if (!empty_element_parsed_guard_(builder_, "sequence_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ',' <<p>>
  private static boolean sequence_1_0(PsiBuilder builder_, int level_, final Parser p) {
    if (!recursion_guard_(builder_, level_, "sequence_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && p.parse(builder_, level_);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // qtycls tyvar
  static boolean simpleclass(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "simpleclass")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycls(builder_, level_ + 1);
    result_ = result_ && tyvar(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // tycon tyvar*
  static boolean simpletype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "simpletype")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = tycon(builder_, level_ + 1);
    result_ = result_ && simpletype_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // tyvar*
  private static boolean simpletype_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "simpletype_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!tyvar(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "simpletype_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // LINE_WS
  static boolean space(PsiBuilder builder_, int level_) {
    return consumeToken(builder_, LINE_WS);
  }

  /* ********************************************************** */
  // '(' | ')' | ',' | ';' | '[' | ']' | '{' | '}' | thquote | backtick
  public static boolean special(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "special")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<special>");
    result_ = consumeToken(builder_, LPAREN);
    if (!result_) result_ = consumeToken(builder_, RPAREN);
    if (!result_) result_ = consumeToken(builder_, COMMA);
    if (!result_) result_ = consumeToken(builder_, SEMICOLON);
    if (!result_) result_ = consumeToken(builder_, LBRACKET);
    if (!result_) result_ = consumeToken(builder_, RBRACKET);
    if (!result_) result_ = consumeToken(builder_, LBRACE);
    if (!result_) result_ = consumeToken(builder_, RBRACE);
    if (!result_) result_ = consumeToken(builder_, THQUOTE);
    if (!result_) result_ = consumeToken(builder_, BACKTICK);
    exit_section_(builder_, level_, marker_, SPECIAL, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
  //          | '\' | '^' | '|' | '-' | '~' | ':'
  public static boolean symbol(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "symbol")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<symbol>");
    result_ = consumeToken(builder_, EXLAMATION);
    if (!result_) result_ = consumeToken(builder_, HASH);
    if (!result_) result_ = consumeToken(builder_, DOLLAR);
    if (!result_) result_ = consumeToken(builder_, PERCENT);
    if (!result_) result_ = consumeToken(builder_, AMPERSAND);
    if (!result_) result_ = consumeToken(builder_, ASTERISK);
    if (!result_) result_ = consumeToken(builder_, PLUS);
    if (!result_) result_ = consumeToken(builder_, PERIOD);
    if (!result_) result_ = consumeToken(builder_, SLASH);
    if (!result_) result_ = consumeToken(builder_, LESSTHAN);
    if (!result_) result_ = consumeToken(builder_, EQUALS);
    if (!result_) result_ = consumeToken(builder_, GREATERTHAN);
    if (!result_) result_ = consumeToken(builder_, QUESTION);
    if (!result_) result_ = consumeToken(builder_, AMPERSAT);
    if (!result_) result_ = consumeToken(builder_, BACKSLASH);
    if (!result_) result_ = consumeToken(builder_, CARET);
    if (!result_) result_ = consumeToken(builder_, PIPE);
    if (!result_) result_ = consumeToken(builder_, MINUS);
    if (!result_) result_ = consumeToken(builder_, TILDE);
    if (!result_) result_ = consumeToken(builder_, COLON);
    exit_section_(builder_, level_, marker_, SYMBOL, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // "type" simpletype "=" typee
  //                   | "data" [context "=>"] simpletype ["=" constrs] [deriving]
  //                   | "newtype" [context "=>"] simpletype "=" newconstr [deriving]
  //                   | "class" [scontext "=>"] tycls tyvar ["where" cdecl+]
  //                   | "instance" [scontext "=>"] qtycls inst ["where" idecl+]
  //                   | "default" <<commaSeparate typee>>
  //                   | "foreign" fdecl
  //                   | decl+
  static boolean topdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = topdecl_0(builder_, level_ + 1);
    if (!result_) result_ = topdecl_1(builder_, level_ + 1);
    if (!result_) result_ = topdecl_2(builder_, level_ + 1);
    if (!result_) result_ = topdecl_3(builder_, level_ + 1);
    if (!result_) result_ = topdecl_4(builder_, level_ + 1);
    if (!result_) result_ = topdecl_5(builder_, level_ + 1);
    if (!result_) result_ = topdecl_6(builder_, level_ + 1);
    if (!result_) result_ = topdecl_7(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "type" simpletype "=" typee
  private static boolean topdecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, TYPE);
    result_ = result_ && simpletype(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, EQUALS);
    result_ = result_ && typee(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "data" [context "=>"] simpletype ["=" constrs] [deriving]
  private static boolean topdecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DATA);
    result_ = result_ && topdecl_1_1(builder_, level_ + 1);
    result_ = result_ && simpletype(builder_, level_ + 1);
    result_ = result_ && topdecl_1_3(builder_, level_ + 1);
    result_ = result_ && topdecl_1_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [context "=>"]
  private static boolean topdecl_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_1_1")) return false;
    topdecl_1_1_0(builder_, level_ + 1);
    return true;
  }

  // context "=>"
  private static boolean topdecl_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = context(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["=" constrs]
  private static boolean topdecl_1_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_1_3")) return false;
    topdecl_1_3_0(builder_, level_ + 1);
    return true;
  }

  // "=" constrs
  private static boolean topdecl_1_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_1_3_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EQUALS);
    result_ = result_ && constrs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [deriving]
  private static boolean topdecl_1_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_1_4")) return false;
    deriving(builder_, level_ + 1);
    return true;
  }

  // "newtype" [context "=>"] simpletype "=" newconstr [deriving]
  private static boolean topdecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, NEWTYPE);
    result_ = result_ && topdecl_2_1(builder_, level_ + 1);
    result_ = result_ && simpletype(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, EQUALS);
    result_ = result_ && newconstr(builder_, level_ + 1);
    result_ = result_ && topdecl_2_5(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [context "=>"]
  private static boolean topdecl_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_2_1")) return false;
    topdecl_2_1_0(builder_, level_ + 1);
    return true;
  }

  // context "=>"
  private static boolean topdecl_2_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_2_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = context(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [deriving]
  private static boolean topdecl_2_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_2_5")) return false;
    deriving(builder_, level_ + 1);
    return true;
  }

  // "class" [scontext "=>"] tycls tyvar ["where" cdecl+]
  private static boolean topdecl_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CLASSTOKEN);
    result_ = result_ && topdecl_3_1(builder_, level_ + 1);
    result_ = result_ && tycls(builder_, level_ + 1);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && topdecl_3_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [scontext "=>"]
  private static boolean topdecl_3_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_3_1")) return false;
    topdecl_3_1_0(builder_, level_ + 1);
    return true;
  }

  // scontext "=>"
  private static boolean topdecl_3_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_3_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = scontext(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["where" cdecl+]
  private static boolean topdecl_3_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_3_4")) return false;
    topdecl_3_4_0(builder_, level_ + 1);
    return true;
  }

  // "where" cdecl+
  private static boolean topdecl_3_4_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_3_4_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, WHERE);
    result_ = result_ && topdecl_3_4_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // cdecl+
  private static boolean topdecl_3_4_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_3_4_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = cdecl(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!cdecl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "topdecl_3_4_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "instance" [scontext "=>"] qtycls inst ["where" idecl+]
  private static boolean topdecl_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, INSTANCE);
    result_ = result_ && topdecl_4_1(builder_, level_ + 1);
    result_ = result_ && qtycls(builder_, level_ + 1);
    result_ = result_ && inst(builder_, level_ + 1);
    result_ = result_ && topdecl_4_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [scontext "=>"]
  private static boolean topdecl_4_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_4_1")) return false;
    topdecl_4_1_0(builder_, level_ + 1);
    return true;
  }

  // scontext "=>"
  private static boolean topdecl_4_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_4_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = scontext(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["where" idecl+]
  private static boolean topdecl_4_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_4_4")) return false;
    topdecl_4_4_0(builder_, level_ + 1);
    return true;
  }

  // "where" idecl+
  private static boolean topdecl_4_4_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_4_4_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, WHERE);
    result_ = result_ && topdecl_4_4_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // idecl+
  private static boolean topdecl_4_4_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_4_4_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = idecl(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!idecl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "topdecl_4_4_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "default" <<commaSeparate typee>>
  private static boolean topdecl_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_5")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DEFAULT);
    result_ = result_ && commaSeparate(builder_, level_ + 1, typee_parser_);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "foreign" fdecl
  private static boolean topdecl_6(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_6")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, FOREIGN);
    result_ = result_ && fdecl(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // decl+
  private static boolean topdecl_7(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_7")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = decl(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!decl(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "topdecl_7", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // conid
  public static boolean tycls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tycls")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONID);
    exit_section_(builder_, marker_, TYCLS, result_);
    return result_;
  }

  /* ********************************************************** */
  // conid
  public static boolean tycon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tycon")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONID);
    exit_section_(builder_, marker_, TYCON, result_);
    return result_;
  }

  /* ********************************************************** */
  // btype ['->' typee]
  public static boolean typee(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typee")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<typee>");
    result_ = btype(builder_, level_ + 1);
    result_ = result_ && typee_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, TYPEE, result_, false, null);
    return result_;
  }

  // ['->' typee]
  private static boolean typee_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typee_1")) return false;
    typee_1_0(builder_, level_ + 1);
    return true;
  }

  // '->' typee
  private static boolean typee_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typee_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, RIGHTARROW);
    result_ = result_ && typee(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // varid
  public static boolean tyvar(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tyvar")) return false;
    if (!nextTokenIs(builder_, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = varid(builder_, level_ + 1);
    exit_section_(builder_, marker_, TYVAR, result_);
    return result_;
  }

  /* ********************************************************** */
  // varid | '(' varsym ')'
  public static boolean var(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "var")) return false;
    if (!nextTokenIs(builder_, "<var>", LPAREN, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<var>");
    result_ = varid(builder_, level_ + 1);
    if (!result_) result_ = var_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, VAR, result_, false, null);
    return result_;
  }

  // '(' varsym ')'
  private static boolean var_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "var_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && varsym(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !reservedid varidRegexp
  static boolean varid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid")) return false;
    if (!nextTokenIs(builder_, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = varid_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, VARIDREGEXP);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !reservedid
  private static boolean varid_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !reservedid(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // varsym | '`' varid '`'
  public static boolean varop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<varop>");
    result_ = varsym(builder_, level_ + 1);
    if (!result_) result_ = varop_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, VAROP, result_, false, null);
    return result_;
  }

  // '`' varid '`'
  private static boolean varop_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varop_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && varid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // <<sequence var>>
  public static boolean vars(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "vars")) return false;
    if (!nextTokenIs(builder_, "<vars>", LPAREN, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<vars>");
    result_ = sequence(builder_, level_ + 1, var_parser_);
    exit_section_(builder_, level_, marker_, VARS, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // !(reservedop | dashes ) ( !':' symbol+ )
  public static boolean varsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<varsym>");
    result_ = varsym_0(builder_, level_ + 1);
    result_ = result_ && varsym_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, VARSYM, result_, false, null);
    return result_;
  }

  // !(reservedop | dashes )
  private static boolean varsym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !varsym_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // reservedop | dashes
  private static boolean varsym_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = reservedop(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, DASHES);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !':' symbol+
  private static boolean varsym_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = varsym_1_0(builder_, level_ + 1);
    result_ = result_ && varsym_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !':'
  private static boolean varsym_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !consumeToken(builder_, COLON);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // symbol+
  private static boolean varsym_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = symbol(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!symbol(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "varsym_1_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // space | newline | '\t'
  public static boolean whitechar(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitechar")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<whitechar>");
    result_ = space(builder_, level_ + 1);
    if (!result_) result_ = newline(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, "\\t");
    exit_section_(builder_, level_, marker_, WHITECHAR, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // whitestuff +
  static boolean whitespace(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitespace")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitestuff(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!whitestuff(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "whitespace", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // whitechar | comment | ncomment | haddock
  static boolean whitestuff(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitestuff")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitechar(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, COMMENT);
    if (!result_) result_ = ncomment(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, HADDOCK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  final static Parser classs_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return classs(builder_, level_ + 1);
    }
  };
  final static Parser cname_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return cname(builder_, level_ + 1);
    }
  };
  final static Parser dclass_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return dclass(builder_, level_ + 1);
    }
  };
  final static Parser export_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return export(builder_, level_ + 1);
    }
  };
  final static Parser importt_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return importt(builder_, level_ + 1);
    }
  };
  final static Parser op_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return op(builder_, level_ + 1);
    }
  };
  final static Parser qvar_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return qvar(builder_, level_ + 1);
    }
  };
  final static Parser simpleclass_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return simpleclass(builder_, level_ + 1);
    }
  };
  final static Parser typee_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return typee(builder_, level_ + 1);
    }
  };
  final static Parser var_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return var(builder_, level_ + 1);
    }
  };
}
