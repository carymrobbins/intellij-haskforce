// This is a generated file. Not intended for manual editing.
package com.haskforce.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.openapi.diagnostic.Logger;
import static com.haskforce.psi.HaskellTypes.*;
import static com.haskforce.psi.HaskellParserUtilBase.*;
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
    if (root_ == AKIND) {
      result_ = akind(builder_, 0);
    }
    else if (root_ == ALTS) {
      result_ = alts(builder_, 0);
    }
    else if (root_ == ATYPE) {
      result_ = atype(builder_, 0);
    }
    else if (root_ == BKIND) {
      result_ = bkind(builder_, 0);
    }
    else if (root_ == BODY) {
      result_ = body(builder_, 0);
    }
    else if (root_ == CDECL) {
      result_ = cdecl(builder_, 0);
    }
    else if (root_ == CLASSDECL) {
      result_ = classdecl(builder_, 0);
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
    else if (root_ == CONID) {
      result_ = conid(builder_, 0);
    }
    else if (root_ == CONOP) {
      result_ = conop(builder_, 0);
    }
    else if (root_ == CONSTR) {
      result_ = constr(builder_, 0);
    }
    else if (root_ == CONSYM) {
      result_ = consym(builder_, 0);
    }
    else if (root_ == CONTEXT) {
      result_ = context(builder_, 0);
    }
    else if (root_ == CPP) {
      result_ = cpp(builder_, 0);
    }
    else if (root_ == CTYPE) {
      result_ = ctype(builder_, 0);
    }
    else if (root_ == DATADECL) {
      result_ = datadecl(builder_, 0);
    }
    else if (root_ == DEFAULTDECL) {
      result_ = defaultdecl(builder_, 0);
    }
    else if (root_ == DERIVINGDECL) {
      result_ = derivingdecl(builder_, 0);
    }
    else if (root_ == EXP) {
      result_ = exp(builder_, 0);
    }
    else if (root_ == EXPORT) {
      result_ = export(builder_, 0);
    }
    else if (root_ == EXPORTS) {
      result_ = exports(builder_, 0);
    }
    else if (root_ == FIXITY) {
      result_ = fixity(builder_, 0);
    }
    else if (root_ == FOREIGNDECL) {
      result_ = foreigndecl(builder_, 0);
    }
    else if (root_ == FTYPE) {
      result_ = ftype(builder_, 0);
    }
    else if (root_ == FUNORPATDECL) {
      result_ = funorpatdecl(builder_, 0);
    }
    else if (root_ == GCONSYM) {
      result_ = gconsym(builder_, 0);
    }
    else if (root_ == GENDECL) {
      result_ = gendecl(builder_, 0);
    }
    else if (root_ == GUARD) {
      result_ = guard(builder_, 0);
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
    else if (root_ == INSTANCEDECL) {
      result_ = instancedecl(builder_, 0);
    }
    else if (root_ == KIND) {
      result_ = kind(builder_, 0);
    }
    else if (root_ == MODULE_PREFIX) {
      result_ = modulePrefix(builder_, 0);
    }
    else if (root_ == MODULEDECL) {
      result_ = moduledecl(builder_, 0);
    }
    else if (root_ == NEWCONSTR) {
      result_ = newconstr(builder_, 0);
    }
    else if (root_ == NEWTYPEDECL) {
      result_ = newtypedecl(builder_, 0);
    }
    else if (root_ == OP) {
      result_ = op(builder_, 0);
    }
    else if (root_ == OPS) {
      result_ = ops(builder_, 0);
    }
    else if (root_ == OQTYCON) {
      result_ = oqtycon(builder_, 0);
    }
    else if (root_ == PAT) {
      result_ = pat(builder_, 0);
    }
    else if (root_ == PPRAGMA) {
      result_ = ppragma(builder_, 0);
    }
    else if (root_ == PSTRINGTOKEN) {
      result_ = pstringtoken(builder_, 0);
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
    else if (root_ == QTYCONOP) {
      result_ = qtyconop(builder_, 0);
    }
    else if (root_ == QTYCONSYM) {
      result_ = qtyconsym(builder_, 0);
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
    else if (root_ == SPECIAL) {
      result_ = special(builder_, 0);
    }
    else if (root_ == STMTS) {
      result_ = stmts(builder_, 0);
    }
    else if (root_ == SYMBOL) {
      result_ = symbol(builder_, 0);
    }
    else if (root_ == TV_BNDR) {
      result_ = tv_bndr(builder_, 0);
    }
    else if (root_ == TYCLS) {
      result_ = tycls(builder_, 0);
    }
    else if (root_ == TYCON) {
      result_ = tycon(builder_, 0);
    }
    else if (root_ == TYCONSYM) {
      result_ = tyconsym(builder_, 0);
    }
    else if (root_ == TYPEDECL) {
      result_ = typedecl(builder_, 0);
    }
    else if (root_ == TYPEE) {
      result_ = typee(builder_, 0);
    }
    else if (root_ == TYVAR) {
      result_ = tyvar(builder_, 0);
    }
    else if (root_ == VARID) {
      result_ = varid(builder_, 0);
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
  // recordlikelhs '{' (fbind ',')* fbind '}'
  //                | [singlequote | '$'] qvar
  //                | thquote qcon
  //                | gcon
  //                | literal
  //                // TODO: Remove semi when lexer supports TH.
  //                | ('[|' | '[' exp '|' [semi]) exp [semi]'|]'
  // //               | "[||" exp '||]'
  //                | '[' "t" '|' ctype '|]'
  //                | '[' "p" '|' infixexp '|]'
  //                | '[' "d" '|' open topdecls close '|]'
  //                | '(#' '#)'
  //                // TODO: Add $( when lexer supports TH.
  //                | '$' '(' exp ')'
  //                | listlike
  //                | parenlike
  static boolean aexp(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = aexp_0(builder_, level_ + 1);
    if (!result_) result_ = aexp_1(builder_, level_ + 1);
    if (!result_) result_ = aexp_2(builder_, level_ + 1);
    if (!result_) result_ = gcon(builder_, level_ + 1);
    if (!result_) result_ = literal(builder_, level_ + 1);
    if (!result_) result_ = aexp_5(builder_, level_ + 1);
    if (!result_) result_ = aexp_6(builder_, level_ + 1);
    if (!result_) result_ = aexp_7(builder_, level_ + 1);
    if (!result_) result_ = aexp_8(builder_, level_ + 1);
    if (!result_) result_ = aexp_9(builder_, level_ + 1);
    if (!result_) result_ = aexp_10(builder_, level_ + 1);
    if (!result_) result_ = listlike(builder_, level_ + 1);
    if (!result_) result_ = parenlike(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // recordlikelhs '{' (fbind ',')* fbind '}'
  private static boolean aexp_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = recordlikelhs(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LBRACE);
    result_ = result_ && aexp_0_2(builder_, level_ + 1);
    result_ = result_ && fbind(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (fbind ',')*
  private static boolean aexp_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_0_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!aexp_0_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "aexp_0_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // fbind ','
  private static boolean aexp_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_0_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fbind(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [singlequote | '$'] qvar
  private static boolean aexp_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = aexp_1_0(builder_, level_ + 1);
    result_ = result_ && qvar(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [singlequote | '$']
  private static boolean aexp_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_1_0")) return false;
    aexp_1_0_0(builder_, level_ + 1);
    return true;
  }

  // singlequote | '$'
  private static boolean aexp_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, SINGLEQUOTE);
    if (!result_) result_ = consumeToken(builder_, DOLLAR);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // thquote qcon
  private static boolean aexp_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, THQUOTE);
    result_ = result_ && qcon(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ('[|' | '[' exp '|' [semi]) exp [semi]'|]'
  private static boolean aexp_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_5")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = aexp_5_0(builder_, level_ + 1);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && aexp_5_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RTHCLOSE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[|' | '[' exp '|' [semi]
  private static boolean aexp_5_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_5_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LTHOPEN);
    if (!result_) result_ = aexp_5_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' exp '|' [semi]
  private static boolean aexp_5_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_5_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PIPE);
    result_ = result_ && aexp_5_0_1_3(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [semi]
  private static boolean aexp_5_0_1_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_5_0_1_3")) return false;
    semi(builder_, level_ + 1);
    return true;
  }

  // [semi]
  private static boolean aexp_5_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_5_2")) return false;
    semi(builder_, level_ + 1);
    return true;
  }

  // '[' "t" '|' ctype '|]'
  private static boolean aexp_6(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_6")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && consumeToken(builder_, "t");
    result_ = result_ && consumeToken(builder_, PIPE);
    result_ = result_ && ctype(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RTHCLOSE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' "p" '|' infixexp '|]'
  private static boolean aexp_7(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_7")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && consumeToken(builder_, "p");
    result_ = result_ && consumeToken(builder_, PIPE);
    result_ = result_ && infixexp(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RTHCLOSE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' "d" '|' open topdecls close '|]'
  private static boolean aexp_8(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_8")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && consumeToken(builder_, "d");
    result_ = result_ && consumeToken(builder_, PIPE);
    result_ = result_ && open(builder_, level_ + 1);
    result_ = result_ && topdecls(builder_, level_ + 1);
    result_ = result_ && close(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RTHCLOSE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(#' '#)'
  private static boolean aexp_9(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_9")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LUNBOXPAREN);
    result_ = result_ && consumeToken(builder_, RUNBOXPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '$' '(' exp ')'
  private static boolean aexp_10(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "aexp_10")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOLLAR);
    result_ = result_ && consumeToken(builder_, LPAREN);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '*'
  //         | '!'
  //         | '(' kind ')'
  //         | pkind
  //         | tyvar
  public static boolean akind(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "akind")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<akind>");
    result_ = consumeToken(builder_, ASTERISK);
    if (!result_) result_ = consumeToken(builder_, EXCLAMATION);
    if (!result_) result_ = akind_2(builder_, level_ + 1);
    if (!result_) result_ = pkind(builder_, level_ + 1);
    if (!result_) result_ = tyvar(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, AKIND, result_, false, null);
    return result_;
  }

  // '(' kind ')'
  private static boolean akind_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "akind_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && kind(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // pat ('->' exp | gdpat) [wheredecls]
  static boolean alt(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "alt")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = pat(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, alt_1(builder_, level_ + 1));
    result_ = pinned_ && alt_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // '->' exp | gdpat
  private static boolean alt_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "alt_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = alt_1_0(builder_, level_ + 1);
    if (!result_) result_ = gdpat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '->' exp
  private static boolean alt_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "alt_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, RIGHTARROW);
    pinned_ = result_; // pin = 1
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [wheredecls]
  private static boolean alt_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "alt_2")) return false;
    wheredecls(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // alt [semi alts]
  public static boolean alts(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "alts")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<alts>");
    result_ = alt(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && alts_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ALTS, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [semi alts]
  private static boolean alts_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "alts_1")) return false;
    alts_1_0(builder_, level_ + 1);
    return true;
  }

  // semi alts
  private static boolean alts_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "alts_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = semi(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && alts(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // open [alts] close
  static boolean altslist(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "altslist")) return false;
    if (!nextTokenIs(builder_, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = open(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, altslist_1(builder_, level_ + 1));
    result_ = pinned_ && close(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [alts]
  private static boolean altslist_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "altslist_1")) return false;
    alts(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // literal
  //                | '_'
  //                | ('!'|'~') apat
  //                | var ('+' integertoken | ['@' apat])
  //                | qcon '{' [(fpat ',')* fpat] '}'
  //                | '(' pat ("->" pat | [',' (pat ',')* pat]) ')'
  //                // Second option is quasiquotes. See TemplateHaskell00002.hs.
  //                | '[' (pat (',' pat)* ']' |  exp '|' [semi] exp [semi]'|]')
  //                | gcon
  static boolean apat(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = literal(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, UNDERSCORE);
    if (!result_) result_ = apat_2(builder_, level_ + 1);
    if (!result_) result_ = apat_3(builder_, level_ + 1);
    if (!result_) result_ = apat_4(builder_, level_ + 1);
    if (!result_) result_ = apat_5(builder_, level_ + 1);
    if (!result_) result_ = apat_6(builder_, level_ + 1);
    if (!result_) result_ = gcon(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ('!'|'~') apat
  private static boolean apat_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat_2_0(builder_, level_ + 1);
    result_ = result_ && apat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '!'|'~'
  private static boolean apat_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EXCLAMATION);
    if (!result_) result_ = consumeToken(builder_, TILDE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // var ('+' integertoken | ['@' apat])
  private static boolean apat_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = var(builder_, level_ + 1);
    result_ = result_ && apat_3_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '+' integertoken | ['@' apat]
  private static boolean apat_3_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_3_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat_3_1_0(builder_, level_ + 1);
    if (!result_) result_ = apat_3_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '+' integertoken
  private static boolean apat_3_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_3_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, PLUS);
    result_ = result_ && consumeToken(builder_, INTEGERTOKEN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['@' apat]
  private static boolean apat_3_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_3_1_1")) return false;
    apat_3_1_1_0(builder_, level_ + 1);
    return true;
  }

  // '@' apat
  private static boolean apat_3_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_3_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, AMPERSAT);
    result_ = result_ && apat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qcon '{' [(fpat ',')* fpat] '}'
  private static boolean apat_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qcon(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LBRACE);
    result_ = result_ && apat_4_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [(fpat ',')* fpat]
  private static boolean apat_4_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_4_2")) return false;
    apat_4_2_0(builder_, level_ + 1);
    return true;
  }

  // (fpat ',')* fpat
  private static boolean apat_4_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_4_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat_4_2_0_0(builder_, level_ + 1);
    result_ = result_ && fpat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (fpat ',')*
  private static boolean apat_4_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_4_2_0_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!apat_4_2_0_0_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "apat_4_2_0_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // fpat ','
  private static boolean apat_4_2_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_4_2_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fpat(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' pat ("->" pat | [',' (pat ',')* pat]) ')'
  private static boolean apat_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_5")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && pat(builder_, level_ + 1);
    result_ = result_ && apat_5_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "->" pat | [',' (pat ',')* pat]
  private static boolean apat_5_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_5_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat_5_2_0(builder_, level_ + 1);
    if (!result_) result_ = apat_5_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "->" pat
  private static boolean apat_5_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_5_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, RIGHTARROW);
    result_ = result_ && pat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [',' (pat ',')* pat]
  private static boolean apat_5_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_5_2_1")) return false;
    apat_5_2_1_0(builder_, level_ + 1);
    return true;
  }

  // ',' (pat ',')* pat
  private static boolean apat_5_2_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_5_2_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && apat_5_2_1_0_1(builder_, level_ + 1);
    result_ = result_ && pat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (pat ',')*
  private static boolean apat_5_2_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_5_2_1_0_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!apat_5_2_1_0_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "apat_5_2_1_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // pat ','
  private static boolean apat_5_2_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_5_2_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pat(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' (pat (',' pat)* ']' |  exp '|' [semi] exp [semi]'|]')
  private static boolean apat_6(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && apat_6_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // pat (',' pat)* ']' |  exp '|' [semi] exp [semi]'|]'
  private static boolean apat_6_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat_6_1_0(builder_, level_ + 1);
    if (!result_) result_ = apat_6_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // pat (',' pat)* ']'
  private static boolean apat_6_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pat(builder_, level_ + 1);
    result_ = result_ && apat_6_1_0_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACKET);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (',' pat)*
  private static boolean apat_6_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6_1_0_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!apat_6_1_0_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "apat_6_1_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ',' pat
  private static boolean apat_6_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && pat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // exp '|' [semi] exp [semi]'|]'
  private static boolean apat_6_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = exp(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PIPE);
    result_ = result_ && apat_6_1_1_2(builder_, level_ + 1);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && apat_6_1_1_4(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RTHCLOSE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [semi]
  private static boolean apat_6_1_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6_1_1_2")) return false;
    semi(builder_, level_ + 1);
    return true;
  }

  // [semi]
  private static boolean apat_6_1_1_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "apat_6_1_1_4")) return false;
    semi(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // "type" (typee [kindsig] | ctype '=' ctype)
  //                  | "data" ctype [kindsig]
  static boolean atdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl")) return false;
    if (!nextTokenIs(builder_, "", DATA, TYPE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atdecl_0(builder_, level_ + 1);
    if (!result_) result_ = atdecl_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "type" (typee [kindsig] | ctype '=' ctype)
  private static boolean atdecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, TYPE);
    result_ = result_ && atdecl_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // typee [kindsig] | ctype '=' ctype
  private static boolean atdecl_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atdecl_0_1_0(builder_, level_ + 1);
    if (!result_) result_ = atdecl_0_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // typee [kindsig]
  private static boolean atdecl_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = typee(builder_, level_ + 1);
    result_ = result_ && atdecl_0_1_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kindsig]
  private static boolean atdecl_0_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl_0_1_0_1")) return false;
    kindsig(builder_, level_ + 1);
    return true;
  }

  // ctype '=' ctype
  private static boolean atdecl_0_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl_0_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ctype(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, EQUALS);
    result_ = result_ && ctype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "data" ctype [kindsig]
  private static boolean atdecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DATA);
    result_ = result_ && ctype(builder_, level_ + 1);
    result_ = result_ && atdecl_1_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kindsig]
  private static boolean atdecl_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atdecl_1_2")) return false;
    kindsig(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // [singlequote] ntgtycon ['##'|'#']
  //         | tyvar
  //         | '{' fielddecls '}'
  //         | '(#' <<sequence ctype>> '#)'
  //         | '(' ['?'] ctype "::" (kind | ctype)')'
  //         | [singlequote] ('(' [<<sequence ctype>>] ')' | '[' <<sequence ctype>> ']')
  //         | integertoken
  //         | pstringtoken
  public static boolean atype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<atype>");
    result_ = atype_0(builder_, level_ + 1);
    if (!result_) result_ = tyvar(builder_, level_ + 1);
    if (!result_) result_ = atype_2(builder_, level_ + 1);
    if (!result_) result_ = atype_3(builder_, level_ + 1);
    if (!result_) result_ = atype_4(builder_, level_ + 1);
    if (!result_) result_ = atype_5(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, INTEGERTOKEN);
    if (!result_) result_ = pstringtoken(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ATYPE, result_, false, null);
    return result_;
  }

  // [singlequote] ntgtycon ['##'|'#']
  private static boolean atype_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atype_0_0(builder_, level_ + 1);
    result_ = result_ && ntgtycon(builder_, level_ + 1);
    result_ = result_ && atype_0_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [singlequote]
  private static boolean atype_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_0_0")) return false;
    consumeToken(builder_, SINGLEQUOTE);
    return true;
  }

  // ['##'|'#']
  private static boolean atype_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_0_2")) return false;
    atype_0_2_0(builder_, level_ + 1);
    return true;
  }

  // '##'|'#'
  private static boolean atype_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_0_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOUBLEHASH);
    if (!result_) result_ = consumeToken(builder_, HASH);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '{' fielddecls '}'
  private static boolean atype_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACE);
    result_ = result_ && fielddecls(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(#' <<sequence ctype>> '#)'
  private static boolean atype_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LUNBOXPAREN);
    result_ = result_ && sequence(builder_, level_ + 1, ctype_parser_);
    result_ = result_ && consumeToken(builder_, RUNBOXPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' ['?'] ctype "::" (kind | ctype)')'
  private static boolean atype_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && atype_4_1(builder_, level_ + 1);
    result_ = result_ && ctype(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && atype_4_4(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['?']
  private static boolean atype_4_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_4_1")) return false;
    consumeToken(builder_, QUESTION);
    return true;
  }

  // kind | ctype
  private static boolean atype_4_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_4_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = kind(builder_, level_ + 1);
    if (!result_) result_ = ctype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [singlequote] ('(' [<<sequence ctype>>] ')' | '[' <<sequence ctype>> ']')
  private static boolean atype_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_5")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atype_5_0(builder_, level_ + 1);
    result_ = result_ && atype_5_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [singlequote]
  private static boolean atype_5_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_5_0")) return false;
    consumeToken(builder_, SINGLEQUOTE);
    return true;
  }

  // '(' [<<sequence ctype>>] ')' | '[' <<sequence ctype>> ']'
  private static boolean atype_5_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_5_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atype_5_1_0(builder_, level_ + 1);
    if (!result_) result_ = atype_5_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' [<<sequence ctype>>] ')'
  private static boolean atype_5_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_5_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && atype_5_1_0_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [<<sequence ctype>>]
  private static boolean atype_5_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_5_1_0_1")) return false;
    sequence(builder_, level_ + 1, ctype_parser_);
    return true;
  }

  // '[' <<sequence ctype>> ']'
  private static boolean atype_5_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atype_5_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && sequence(builder_, level_ + 1, ctype_parser_);
    result_ = result_ && consumeToken(builder_, RBRACKET);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // akind [bkind]
  public static boolean bkind(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bkind")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<bkind>");
    result_ = akind(builder_, level_ + 1);
    result_ = result_ && bkind_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, BKIND, result_, false, null);
    return result_;
  }

  // [bkind]
  private static boolean bkind_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bkind_1")) return false;
    bkind(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // (cpp | ppragma)* open [bodyaux] close
  public static boolean body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "body")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<body>");
    result_ = body_0(builder_, level_ + 1);
    result_ = result_ && open(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, body_2(builder_, level_ + 1));
    result_ = pinned_ && close(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, BODY, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (cpp | ppragma)*
  private static boolean body_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "body_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!body_0_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "body_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // cpp | ppragma
  private static boolean body_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "body_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = cpp(builder_, level_ + 1);
    if (!result_) result_ = ppragma(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [bodyaux]
  private static boolean body_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "body_2")) return false;
    bodyaux(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // impdecls [semi topdecls]
  //                   | [impdecls semi] topdecls
  static boolean bodyaux(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bodyaux")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = bodyaux_0(builder_, level_ + 1);
    if (!result_) result_ = bodyaux_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // impdecls [semi topdecls]
  private static boolean bodyaux_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bodyaux_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = impdecls(builder_, level_ + 1);
    result_ = result_ && bodyaux_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [semi topdecls]
  private static boolean bodyaux_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bodyaux_0_1")) return false;
    bodyaux_0_1_0(builder_, level_ + 1);
    return true;
  }

  // semi topdecls
  private static boolean bodyaux_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bodyaux_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = semi(builder_, level_ + 1);
    result_ = result_ && topdecls(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [impdecls semi] topdecls
  private static boolean bodyaux_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bodyaux_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = bodyaux_1_0(builder_, level_ + 1);
    result_ = result_ && topdecls(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [impdecls semi]
  private static boolean bodyaux_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bodyaux_1_0")) return false;
    bodyaux_1_0_0(builder_, level_ + 1);
    return true;
  }

  // impdecls semi
  private static boolean bodyaux_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bodyaux_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = impdecls(builder_, level_ + 1);
    result_ = result_ && semi(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // atype [btype]
  static boolean btype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "btype")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atype(builder_, level_ + 1);
    result_ = result_ && btype_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [btype]
  private static boolean btype_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "btype_1")) return false;
    btype(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // "ccall" | "stdcall" | "cplusplus"
  //                    | "jvm" | "dotnet"
  static boolean callconv(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "callconv")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "ccall");
    if (!result_) result_ = consumeToken(builder_, "stdcall");
    if (!result_) result_ = consumeToken(builder_, "cplusplus");
    if (!result_) result_ = consumeToken(builder_, "jvm");
    if (!result_) result_ = consumeToken(builder_, "dotnet");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // atdecl
  //         | (funlhs | var) rhs
  //         | gendecl
  public static boolean cdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<cdecl>");
    result_ = atdecl(builder_, level_ + 1);
    if (!result_) result_ = cdecl_1(builder_, level_ + 1);
    if (!result_) result_ = gendecl(builder_, level_ + 1);
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
  // open [cdecls1] close
  static boolean cdecls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecls")) return false;
    if (!nextTokenIs(builder_, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = open(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, cdecls_1(builder_, level_ + 1));
    result_ = pinned_ && close(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [cdecls1]
  private static boolean cdecls_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecls_1")) return false;
    cdecls1(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // cdecl [semi cdecls1]
  static boolean cdecls1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecls1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = cdecl(builder_, level_ + 1);
    result_ = result_ && cdecls1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [semi cdecls1]
  private static boolean cdecls1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecls1_1")) return false;
    cdecls1_1_0(builder_, level_ + 1);
    return true;
  }

  // semi cdecls1
  private static boolean cdecls1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cdecls1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = semi(builder_, level_ + 1);
    result_ = result_ && cdecls1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "class" ctype ["where" cdecls]
  public static boolean classdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "classdecl")) return false;
    if (!nextTokenIs(builder_, CLASSTOKEN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, CLASSTOKEN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, ctype(builder_, level_ + 1));
    result_ = pinned_ && classdecl_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, CLASSDECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["where" cdecls]
  private static boolean classdecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "classdecl_2")) return false;
    classdecl_2_0(builder_, level_ + 1);
    return true;
  }

  // "where" cdecls
  private static boolean classdecl_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "classdecl_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, WHERE);
    result_ = result_ && cdecls(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '}'
  //                 | WHITESPACERBRACETOK
  static boolean close(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "close")) return false;
    if (!nextTokenIs(builder_, "", RBRACE, WHITESPACERBRACETOK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, RBRACE);
    if (!result_) result_ = consumeToken(builder_, WHITESPACERBRACETOK);
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
  // kind  [',' comma_kinds1]
  static boolean comma_kinds1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comma_kinds1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = kind(builder_, level_ + 1);
    result_ = result_ && comma_kinds1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [',' comma_kinds1]
  private static boolean comma_kinds1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comma_kinds1_1")) return false;
    comma_kinds1_1_0(builder_, level_ + 1);
    return true;
  }

  // ',' comma_kinds1
  private static boolean comma_kinds1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comma_kinds1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && comma_kinds1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ',' (',')*
  static boolean commas(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "commas")) return false;
    if (!nextTokenIs(builder_, COMMA)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && commas_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (',')*
  private static boolean commas_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "commas_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!commas_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "commas_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // (',')
  private static boolean commas_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "commas_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // conidRegexp | '(' consym ')'
  public static boolean con(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "con")) return false;
    if (!nextTokenIs(builder_, "<con>", LPAREN, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<con>");
    result_ = consumeToken(builder_, CONIDREGEXP);
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
  // conidRegexp
  public static boolean conid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "conid")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONIDREGEXP);
    exit_section_(builder_, marker_, CONID, result_);
    return result_;
  }

  /* ********************************************************** */
  // consym | '`' conid '`'
  public static boolean conop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "conop")) return false;
    if (!nextTokenIs(builder_, "<conop>", BACKTICK, CONSYMTOK)) return false;
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
    result_ = result_ && conid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // (btype | '!'atype) conop (btype | '!'atype)
  //          | con '{' (fielddecl ',')* fielddecl '}'
  //          | con ([ppragma] ['!'] atype)*
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

  // (btype | '!'atype) conop (btype | '!'atype)
  private static boolean constr_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = constr_0_0(builder_, level_ + 1);
    result_ = result_ && conop(builder_, level_ + 1);
    result_ = result_ && constr_0_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // btype | '!'atype
  private static boolean constr_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = btype(builder_, level_ + 1);
    if (!result_) result_ = constr_0_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '!'atype
  private static boolean constr_0_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EXCLAMATION);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // btype | '!'atype
  private static boolean constr_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = btype(builder_, level_ + 1);
    if (!result_) result_ = constr_0_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '!'atype
  private static boolean constr_0_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_0_2_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EXCLAMATION);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // con '{' (fielddecl ',')* fielddecl '}'
  private static boolean constr_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = con(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LBRACE);
    result_ = result_ && constr_1_2(builder_, level_ + 1);
    result_ = result_ && fielddecl(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (fielddecl ',')*
  private static boolean constr_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!constr_1_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "constr_1_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // fielddecl ','
  private static boolean constr_1_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_1_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fielddecl(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // con ([ppragma] ['!'] atype)*
  private static boolean constr_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = con(builder_, level_ + 1);
    result_ = result_ && constr_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ([ppragma] ['!'] atype)*
  private static boolean constr_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!constr_2_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "constr_2_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // [ppragma] ['!'] atype
  private static boolean constr_2_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = constr_2_1_0_0(builder_, level_ + 1);
    result_ = result_ && constr_2_1_0_1(builder_, level_ + 1);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [ppragma]
  private static boolean constr_2_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_1_0_0")) return false;
    ppragma(builder_, level_ + 1);
    return true;
  }

  // ['!']
  private static boolean constr_2_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constr_2_1_0_1")) return false;
    consumeToken(builder_, EXCLAMATION);
    return true;
  }

  /* ********************************************************** */
  // constr ('|' constr)*
  static boolean constrs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "constrs")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = constr(builder_, level_ + 1);
    result_ = result_ && constrs_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
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
  // CONSYMTOK
  public static boolean consym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym")) return false;
    if (!nextTokenIs(builder_, CONSYMTOK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONSYMTOK);
    exit_section_(builder_, marker_, CONSYM, result_);
    return result_;
  }

  /* ********************************************************** */
  // btype ['~' btype]
  public static boolean context(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "context")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<context>");
    result_ = btype(builder_, level_ + 1);
    result_ = result_ && context_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CONTEXT, result_, false, null);
    return result_;
  }

  // ['~' btype]
  private static boolean context_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "context_1")) return false;
    context_1_0(builder_, level_ + 1);
    return true;
  }

  // '~' btype
  private static boolean context_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "context_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, TILDE);
    result_ = result_ && btype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // context '=>' ctype
  static boolean contexttype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "contexttype")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = context(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    pinned_ = result_; // pin = 2
    result_ = result_ && ctype(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // CPPIF | CPPELSE | CPPENDIF
  public static boolean cpp(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cpp")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<cpp>");
    result_ = consumeToken(builder_, CPPIF);
    if (!result_) result_ = consumeToken(builder_, CPPELSE);
    if (!result_) result_ = consumeToken(builder_, CPPENDIF);
    exit_section_(builder_, level_, marker_, CPP, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // foralltype
  //         | contexttype
  //         | typee
  public static boolean ctype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ctype")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<ctype>");
    result_ = foralltype(builder_, level_ + 1);
    if (!result_) result_ = contexttype(builder_, level_ + 1);
    if (!result_) result_ = typee(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, CTYPE, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // "data" ["instance"] [context "=>"] typee ['=' constrs| [kindsig] ["where" gadtconstrs]] [deriving]
  public static boolean datadecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl")) return false;
    if (!nextTokenIs(builder_, DATA)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, DATA);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, datadecl_1(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, datadecl_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, typee(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, datadecl_4(builder_, level_ + 1)) && result_;
    result_ = pinned_ && datadecl_5(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, DATADECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["instance"]
  private static boolean datadecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_1")) return false;
    consumeToken(builder_, INSTANCE);
    return true;
  }

  // [context "=>"]
  private static boolean datadecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_2")) return false;
    datadecl_2_0(builder_, level_ + 1);
    return true;
  }

  // context "=>"
  private static boolean datadecl_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = context(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['=' constrs| [kindsig] ["where" gadtconstrs]]
  private static boolean datadecl_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_4")) return false;
    datadecl_4_0(builder_, level_ + 1);
    return true;
  }

  // '=' constrs| [kindsig] ["where" gadtconstrs]
  private static boolean datadecl_4_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_4_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = datadecl_4_0_0(builder_, level_ + 1);
    if (!result_) result_ = datadecl_4_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '=' constrs
  private static boolean datadecl_4_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_4_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EQUALS);
    result_ = result_ && constrs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kindsig] ["where" gadtconstrs]
  private static boolean datadecl_4_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_4_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = datadecl_4_0_1_0(builder_, level_ + 1);
    result_ = result_ && datadecl_4_0_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kindsig]
  private static boolean datadecl_4_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_4_0_1_0")) return false;
    kindsig(builder_, level_ + 1);
    return true;
  }

  // ["where" gadtconstrs]
  private static boolean datadecl_4_0_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_4_0_1_1")) return false;
    datadecl_4_0_1_1_0(builder_, level_ + 1);
    return true;
  }

  // "where" gadtconstrs
  private static boolean datadecl_4_0_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_4_0_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, WHERE);
    result_ = result_ && gadtconstrs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [deriving]
  private static boolean datadecl_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "datadecl_5")) return false;
    deriving(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // qtycls
  static boolean dclass(PsiBuilder builder_, int level_) {
    return qtycls(builder_, level_ + 1);
  }

  /* ********************************************************** */
  // funorpatdecl
  //                | ppragma
  //                | gendecl
  static boolean decl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = funorpatdecl(builder_, level_ + 1);
    if (!result_) result_ = ppragma(builder_, level_ + 1);
    if (!result_) result_ = gendecl(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // open [decls1] close
  static boolean decls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decls")) return false;
    if (!nextTokenIs(builder_, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = open(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, decls_1(builder_, level_ + 1));
    result_ = pinned_ && close(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [decls1]
  private static boolean decls_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decls_1")) return false;
    decls1(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // decl [semi decls1]
  static boolean decls1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decls1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = decl(builder_, level_ + 1);
    result_ = result_ && decls1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [semi decls1]
  private static boolean decls1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decls1_1")) return false;
    decls1_1_0(builder_, level_ + 1);
    return true;
  }

  // semi decls1
  private static boolean decls1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decls1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = semi(builder_, level_ + 1);
    result_ = result_ && decls1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "default" <<commaSeparate typee>>
  public static boolean defaultdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "defaultdecl")) return false;
    if (!nextTokenIs(builder_, DEFAULT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, DEFAULT);
    pinned_ = result_; // pin = 1
    result_ = result_ && commaSeparate(builder_, level_ + 1, typee_parser_);
    exit_section_(builder_, level_, marker_, DEFAULTDECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // "deriving" (dclass|<<commaSeparate dclass>>)
  static boolean deriving(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "deriving")) return false;
    if (!nextTokenIs(builder_, DERIVING)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, DERIVING);
    pinned_ = result_; // pin = 1
    result_ = result_ && deriving_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
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
  // "deriving" "instance" [ppragma] ctype
  public static boolean derivingdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "derivingdecl")) return false;
    if (!nextTokenIs(builder_, DERIVING)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, DERIVING);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, INSTANCE));
    result_ = pinned_ && report_error_(builder_, derivingdecl_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && ctype(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, DERIVINGDECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [ppragma]
  private static boolean derivingdecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "derivingdecl_2")) return false;
    ppragma(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // infixexp ["::" [context "=>"] typee]
  public static boolean exp(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exp")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<exp>");
    result_ = infixexp(builder_, level_ + 1);
    result_ = result_ && exp_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, EXP, result_, false, null);
    return result_;
  }

  // ["::" [context "=>"] typee]
  private static boolean exp_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exp_1")) return false;
    exp_1_0(builder_, level_ + 1);
    return true;
  }

  // "::" [context "=>"] typee
  private static boolean exp_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exp_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && exp_1_0_1(builder_, level_ + 1);
    result_ = result_ && typee(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [context "=>"]
  private static boolean exp_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exp_1_0_1")) return false;
    exp_1_0_1_0(builder_, level_ + 1);
    return true;
  }

  // context "=>"
  private static boolean exp_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exp_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = context(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // pstringtoken
  static boolean expent(PsiBuilder builder_, int level_) {
    return pstringtoken(builder_, level_ + 1);
  }

  /* ********************************************************** */
  // cpp+ export
  //         | qvar
  //         | qtycon ["(..)" | cnames]
  //         | qtycls ["(..)" | qvars]
  //         | "module" qconid
  public static boolean export(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<export>");
    result_ = export_0(builder_, level_ + 1);
    if (!result_) result_ = qvar(builder_, level_ + 1);
    if (!result_) result_ = export_2(builder_, level_ + 1);
    if (!result_) result_ = export_3(builder_, level_ + 1);
    if (!result_) result_ = export_4(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, EXPORT, result_, false, null);
    return result_;
  }

  // cpp+ export
  private static boolean export_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = export_0_0(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && export(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // cpp+
  private static boolean export_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = cpp(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!cpp(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "export_0_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qtycon ["(..)" | cnames]
  private static boolean export_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_2")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = qtycon(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && export_2_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["(..)" | cnames]
  private static boolean export_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_2_1")) return false;
    export_2_1_0(builder_, level_ + 1);
    return true;
  }

  // "(..)" | cnames
  private static boolean export_2_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_2_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "(..)");
    if (!result_) result_ = cnames(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qtycls ["(..)" | qvars]
  private static boolean export_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_3")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = qtycls(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && export_3_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["(..)" | qvars]
  private static boolean export_3_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_3_1")) return false;
    export_3_1_0(builder_, level_ + 1);
    return true;
  }

  // "(..)" | qvars
  private static boolean export_3_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_3_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "(..)");
    if (!result_) result_ = qvars(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "module" qconid
  private static boolean export_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_4")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, MODULETOKEN);
    pinned_ = result_; // pin = 1
    result_ = result_ && qconid(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // "export" callconv [expent]
  static boolean exportdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exportdecl")) return false;
    if (!nextTokenIs(builder_, EXPORTTOKEN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, EXPORTTOKEN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, callconv(builder_, level_ + 1));
    result_ = pinned_ && exportdecl_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [expent]
  private static boolean exportdecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exportdecl_2")) return false;
    expent(builder_, level_ + 1);
    return true;
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
  // qtycon atype*
  static boolean fatype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fatype")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycon(builder_, level_ + 1);
    result_ = result_ && fatype_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // atype*
  private static boolean fatype_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fatype_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!atype(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "fatype_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // qvar '=' exp
  static boolean fbind(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fbind")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = qvar(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, EQUALS);
    pinned_ = result_; // pin = 2
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // (importdecl | exportdecl) var "::" ftype
  static boolean fdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fdecl")) return false;
    if (!nextTokenIs(builder_, "", EXPORTTOKEN, IMPORT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fdecl_0(builder_, level_ + 1);
    result_ = result_ && var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && ftype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // importdecl | exportdecl
  private static boolean fdecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fdecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = importdecl(builder_, level_ + 1);
    if (!result_) result_ = exportdecl(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // aexp [fexp]
  static boolean fexp(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fexp")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = aexp(builder_, level_ + 1);
    result_ = result_ && fexp_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [fexp]
  private static boolean fexp_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fexp_1")) return false;
    fexp(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // vars "::" (typee | '!' atype)
  static boolean fielddecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fielddecl")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = vars(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    pinned_ = result_; // pin = 2
    result_ = result_ && fielddecl_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
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
    result_ = consumeToken(builder_, EXCLAMATION);
    result_ = result_ && atype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // fielddecl [',' fielddecls]
  static boolean fielddecls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fielddecls")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = fielddecl(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && fielddecls_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [',' fielddecls]
  private static boolean fielddecls_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fielddecls_1")) return false;
    fielddecls_1_0(builder_, level_ + 1);
    return true;
  }

  // ',' fielddecls
  private static boolean fielddecls_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fielddecls_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && fielddecls(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
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
  // "forall" tv_bndr* '.' ctype
  static boolean foralltype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "foralltype")) return false;
    if (!nextTokenIs(builder_, FORALLTOKEN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, FORALLTOKEN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, foralltype_1(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, PERIOD)) && result_;
    result_ = pinned_ && ctype(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // tv_bndr*
  private static boolean foralltype_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "foralltype_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!tv_bndr(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "foralltype_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // "foreign" fdecl
  public static boolean foreigndecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "foreigndecl")) return false;
    if (!nextTokenIs(builder_, FOREIGN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, FOREIGN);
    pinned_ = result_; // pin = 1
    result_ = result_ && fdecl(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, FOREIGNDECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // qvar ['=' pat]
  //                | ".."
  static boolean fpat(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fpat")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fpat_0(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, DOUBLEPERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qvar ['=' pat]
  private static boolean fpat_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fpat_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = qvar(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && fpat_0_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['=' pat]
  private static boolean fpat_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fpat_0_1")) return false;
    fpat_0_1_0(builder_, level_ + 1);
    return true;
  }

  // '=' pat
  private static boolean fpat_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fpat_0_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, EQUALS);
    pinned_ = result_; // pin = 1
    result_ = result_ && pat(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // fatype ["->" ftype]
  //         | "()"
  public static boolean ftype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ftype")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<ftype>");
    result_ = ftype_0(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, "()");
    exit_section_(builder_, level_, marker_, FTYPE, result_, false, null);
    return result_;
  }

  // fatype ["->" ftype]
  private static boolean ftype_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ftype_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fatype(builder_, level_ + 1);
    result_ = result_ && ftype_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["->" ftype]
  private static boolean ftype_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ftype_0_1")) return false;
    ftype_0_1_0(builder_, level_ + 1);
    return true;
  }

  // "->" ftype
  private static boolean ftype_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ftype_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, RIGHTARROW);
    result_ = result_ && ftype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // (var |'(' funlhs ')') apat+
  //                  | pat varop pat
  static boolean funlhs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funlhs")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = funlhs_0(builder_, level_ + 1);
    if (!result_) result_ = funlhs_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (var |'(' funlhs ')') apat+
  private static boolean funlhs_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funlhs_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = funlhs_0_0(builder_, level_ + 1);
    result_ = result_ && funlhs_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // var |'(' funlhs ')'
  private static boolean funlhs_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funlhs_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = var(builder_, level_ + 1);
    if (!result_) result_ = funlhs_0_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' funlhs ')'
  private static boolean funlhs_0_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funlhs_0_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && funlhs(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // apat+
  private static boolean funlhs_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funlhs_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!apat(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "funlhs_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // pat varop pat
  private static boolean funlhs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funlhs_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pat(builder_, level_ + 1);
    result_ = result_ && varop(builder_, level_ + 1);
    result_ = result_ && pat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // (funlhs | pat) rhs
  public static boolean funorpatdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funorpatdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<funorpatdecl>");
    result_ = funorpatdecl_0(builder_, level_ + 1);
    result_ = result_ && rhs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, FUNORPATDECL, result_, false, null);
    return result_;
  }

  // funlhs | pat
  private static boolean funorpatdecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "funorpatdecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = funlhs(builder_, level_ + 1);
    if (!result_) result_ = pat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // <<sequence con>> "::" ctype
  //                      | oqtycon '{' fielddecls '}' "::" ctype
  static boolean gadtconstr(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gadtconstr")) return false;
    if (!nextTokenIs(builder_, "", LPAREN, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = gadtconstr_0(builder_, level_ + 1);
    if (!result_) result_ = gadtconstr_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // <<sequence con>> "::" ctype
  private static boolean gadtconstr_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gadtconstr_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = sequence(builder_, level_ + 1, con_parser_);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && ctype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // oqtycon '{' fielddecls '}' "::" ctype
  private static boolean gadtconstr_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gadtconstr_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = oqtycon(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LBRACE);
    result_ = result_ && fielddecls(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACE);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && ctype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // open [gadtconstrs1] close
  static boolean gadtconstrs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gadtconstrs")) return false;
    if (!nextTokenIs(builder_, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = open(builder_, level_ + 1);
    result_ = result_ && gadtconstrs_1(builder_, level_ + 1);
    result_ = result_ && close(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [gadtconstrs1]
  private static boolean gadtconstrs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gadtconstrs_1")) return false;
    gadtconstrs1(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // gadtconstr semi gadtconstrs1
  //                        | gadtconstr
  static boolean gadtconstrs1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gadtconstrs1")) return false;
    if (!nextTokenIs(builder_, "", LPAREN, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = gadtconstrs1_0(builder_, level_ + 1);
    if (!result_) result_ = gadtconstr(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // gadtconstr semi gadtconstrs1
  private static boolean gadtconstrs1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gadtconstrs1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = gadtconstr(builder_, level_ + 1);
    result_ = result_ && semi(builder_, level_ + 1);
    result_ = result_ && gadtconstrs1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '[' ']'
  //               | '(' [',' (',')*] ')'
  //               | qcon
  static boolean gcon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gcon")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = gcon_0(builder_, level_ + 1);
    if (!result_) result_ = gcon_1(builder_, level_ + 1);
    if (!result_) result_ = qcon(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' ']'
  private static boolean gcon_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gcon_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && consumeToken(builder_, RBRACKET);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' [',' (',')*] ')'
  private static boolean gcon_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gcon_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && gcon_1_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [',' (',')*]
  private static boolean gcon_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gcon_1_1")) return false;
    gcon_1_1_0(builder_, level_ + 1);
    return true;
  }

  // ',' (',')*
  private static boolean gcon_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gcon_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && gcon_1_1_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (',')*
  private static boolean gcon_1_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gcon_1_1_0_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!gcon_1_1_0_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "gcon_1_1_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // (',')
  private static boolean gcon_1_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gcon_1_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ':' | qconsym
  public static boolean gconsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gconsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<gconsym>");
    result_ = consumeToken(builder_, COLON);
    if (!result_) result_ = qconsym(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, GCONSYM, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // guards '->' exp [gdpat]
  static boolean gdpat(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gdpat")) return false;
    if (!nextTokenIs(builder_, PIPE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = guards(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RIGHTARROW);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, exp(builder_, level_ + 1));
    result_ = pinned_ && gdpat_3(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [gdpat]
  private static boolean gdpat_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gdpat_3")) return false;
    gdpat(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // guards '=' exp
  static boolean gdrhs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gdrhs")) return false;
    if (!nextTokenIs(builder_, PIPE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = guards(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, EQUALS);
    pinned_ = result_; // pin = 2
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // gendeclfst
  //           | fixity [integertoken] ops
  public static boolean gendecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<gendecl>");
    result_ = gendeclfst(builder_, level_ + 1);
    if (!result_) result_ = gendecl_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, GENDECL, result_, false, null);
    return result_;
  }

  // fixity [integertoken] ops
  private static boolean gendecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = fixity(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, gendecl_1_1(builder_, level_ + 1));
    result_ = pinned_ && ops(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [integertoken]
  private static boolean gendecl_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendecl_1_1")) return false;
    consumeToken(builder_, INTEGERTOKEN);
    return true;
  }

  /* ********************************************************** */
  // vars '::' ctype
  static boolean gendeclfst(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gendeclfst")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = vars(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLECOLON);
    pinned_ = result_; // pin = 2
    result_ = result_ && ctype(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // pat '<-' infixexp
  //         | "let" decls
  //         | infixexp
  public static boolean guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<guard>");
    result_ = guard_0(builder_, level_ + 1);
    if (!result_) result_ = guard_1(builder_, level_ + 1);
    if (!result_) result_ = infixexp(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, GUARD, result_, false, null);
    return result_;
  }

  // pat '<-' infixexp
  private static boolean guard_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pat(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LEFTARROW);
    result_ = result_ && infixexp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "let" decls
  private static boolean guard_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LET);
    result_ = result_ && decls(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '|' guard (',' guard)*
  static boolean guards(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guards")) return false;
    if (!nextTokenIs(builder_, PIPE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, PIPE);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, guard(builder_, level_ + 1));
    result_ = pinned_ && guards_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' guard)*
  private static boolean guards_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guards_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!guards_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "guards_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ',' guard
  private static boolean guards_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guards_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && guard(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // ppragma+ idecl
  //         | itdecl
  //         | (funlhs | var) rhs
  public static boolean idecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<idecl>");
    result_ = idecl_0(builder_, level_ + 1);
    if (!result_) result_ = itdecl(builder_, level_ + 1);
    if (!result_) result_ = idecl_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, IDECL, result_, false, null);
    return result_;
  }

  // ppragma+ idecl
  private static boolean idecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = idecl_0_0(builder_, level_ + 1);
    result_ = result_ && idecl(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ppragma+
  private static boolean idecl_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ppragma(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!ppragma(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "idecl_0_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (funlhs | var) rhs
  private static boolean idecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = idecl_2_0(builder_, level_ + 1);
    result_ = result_ && rhs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // funlhs | var
  private static boolean idecl_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = funlhs(builder_, level_ + 1);
    if (!result_) result_ = var(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // idecl ppragma*
  static boolean idecl0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = idecl(builder_, level_ + 1);
    result_ = result_ && idecl0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ppragma*
  private static boolean idecl0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecl0_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!ppragma(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "idecl0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // ppragma+ idecls
  //                  | open [idecls1] close
  static boolean idecls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = idecls_0(builder_, level_ + 1);
    if (!result_) result_ = idecls_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ppragma+ idecls
  private static boolean idecls_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = idecls_0_0(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && idecls(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ppragma+
  private static boolean idecls_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ppragma(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!ppragma(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "idecls_0_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // open [idecls1] close
  private static boolean idecls_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = open(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, idecls_1_1(builder_, level_ + 1));
    result_ = pinned_ && close(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [idecls1]
  private static boolean idecls_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls_1_1")) return false;
    idecls1(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // idecl0 [semi idecls1]
  static boolean idecls1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = idecl0(builder_, level_ + 1);
    result_ = result_ && idecls1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [semi idecls1]
  private static boolean idecls1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls1_1")) return false;
    idecls1_1_0(builder_, level_ + 1);
    return true;
  }

  // semi idecls1
  private static boolean idecls1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "idecls1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = semi(builder_, level_ + 1);
    result_ = result_ && idecls1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "import" ["qualified"] qconid ["as" qconid] [impspec] [cpp]
  public static boolean impdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl")) return false;
    if (!nextTokenIs(builder_, IMPORT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, IMPORT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, impdecl_1(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, qconid(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, impdecl_3(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, impdecl_4(builder_, level_ + 1)) && result_;
    result_ = pinned_ && impdecl_5(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, IMPDECL, result_, pinned_, null);
    return result_ || pinned_;
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
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, AS);
    pinned_ = result_; // pin = 1
    result_ = result_ && qconid(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [impspec]
  private static boolean impdecl_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_4")) return false;
    impspec(builder_, level_ + 1);
    return true;
  }

  // [cpp]
  private static boolean impdecl_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_5")) return false;
    cpp(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // impdecl [semi impdecls]
  static boolean impdecls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecls")) return false;
    if (!nextTokenIs(builder_, IMPORT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = impdecl(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && impdecls_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [semi impdecls]
  private static boolean impdecls_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecls_1")) return false;
    impdecls_1_0(builder_, level_ + 1);
    return true;
  }

  // semi impdecls
  private static boolean impdecls_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecls_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = semi(builder_, level_ + 1);
    result_ = result_ && impdecls(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '\"' "wrapper" '\"'
  //                  | '\"' "dynamic" '\"'
  //                  | pstringtoken
  static boolean impent(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impent")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = impent_0(builder_, level_ + 1);
    if (!result_) result_ = impent_1(builder_, level_ + 1);
    if (!result_) result_ = pstringtoken(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '\"' "wrapper" '\"'
  private static boolean impent_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impent_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "\\\"");
    result_ = result_ && consumeToken(builder_, "wrapper");
    result_ = result_ && consumeToken(builder_, "\\\"");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '\"' "dynamic" '\"'
  private static boolean impent_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impent_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "\\\"");
    result_ = result_ && consumeToken(builder_, "dynamic");
    result_ = result_ && consumeToken(builder_, "\\\"");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "import" callconv [safety] impent
  static boolean importdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importdecl")) return false;
    if (!nextTokenIs(builder_, IMPORT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, IMPORT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, callconv(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, importdecl_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && impent(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [safety]
  private static boolean importdecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "importdecl_2")) return false;
    safety(builder_, level_ + 1);
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
  // ["hiding"] '(' [<<sequence importt>>] ')'
  static boolean impspec(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impspec")) return false;
    if (!nextTokenIs(builder_, "", LPAREN, HIDING)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = impspec_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LPAREN);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, impspec_2(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, RPAREN) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["hiding"]
  private static boolean impspec_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impspec_0")) return false;
    consumeToken(builder_, HIDING);
    return true;
  }

  // [<<sequence importt>>]
  private static boolean impspec_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impspec_2")) return false;
    sequence(builder_, level_ + 1, importt_parser_);
    return true;
  }

  /* ********************************************************** */
  // '-'* lexp [qop infixexp]
  static boolean infixexp(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "infixexp")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = infixexp_0(builder_, level_ + 1);
    result_ = result_ && lexp(builder_, level_ + 1);
    result_ = result_ && infixexp_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '-'*
  private static boolean infixexp_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "infixexp_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consumeToken(builder_, MINUS)) break;
      if (!empty_element_parsed_guard_(builder_, "infixexp_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // [qop infixexp]
  private static boolean infixexp_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "infixexp_2")) return false;
    infixexp_2_0(builder_, level_ + 1);
    return true;
  }

  // qop infixexp
  private static boolean infixexp_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "infixexp_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qop(builder_, level_ + 1);
    result_ = result_ && infixexp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "instance" ctype ["where" idecls]
  public static boolean instancedecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "instancedecl")) return false;
    if (!nextTokenIs(builder_, INSTANCE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, INSTANCE);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, ctype(builder_, level_ + 1));
    result_ = pinned_ && instancedecl_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, INSTANCEDECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["where" idecls]
  private static boolean instancedecl_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "instancedecl_2")) return false;
    instancedecl_2_0(builder_, level_ + 1);
    return true;
  }

  // "where" idecls
  private static boolean instancedecl_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "instancedecl_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, WHERE);
    result_ = result_ && idecls(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "type" ctype '=' ctype
  //                  | ("data" | "newtype") ctype ([kindsig] gadtconstrs | ['=' constrs]) [deriving]
  static boolean itdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = itdecl_0(builder_, level_ + 1);
    if (!result_) result_ = itdecl_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "type" ctype '=' ctype
  private static boolean itdecl_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, TYPE);
    result_ = result_ && ctype(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, EQUALS);
    result_ = result_ && ctype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ("data" | "newtype") ctype ([kindsig] gadtconstrs | ['=' constrs]) [deriving]
  private static boolean itdecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = itdecl_1_0(builder_, level_ + 1);
    result_ = result_ && ctype(builder_, level_ + 1);
    result_ = result_ && itdecl_1_2(builder_, level_ + 1);
    result_ = result_ && itdecl_1_3(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "data" | "newtype"
  private static boolean itdecl_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DATA);
    if (!result_) result_ = consumeToken(builder_, NEWTYPE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kindsig] gadtconstrs | ['=' constrs]
  private static boolean itdecl_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = itdecl_1_2_0(builder_, level_ + 1);
    if (!result_) result_ = itdecl_1_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kindsig] gadtconstrs
  private static boolean itdecl_1_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = itdecl_1_2_0_0(builder_, level_ + 1);
    result_ = result_ && gadtconstrs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kindsig]
  private static boolean itdecl_1_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1_2_0_0")) return false;
    kindsig(builder_, level_ + 1);
    return true;
  }

  // ['=' constrs]
  private static boolean itdecl_1_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1_2_1")) return false;
    itdecl_1_2_1_0(builder_, level_ + 1);
    return true;
  }

  // '=' constrs
  private static boolean itdecl_1_2_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1_2_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EQUALS);
    result_ = result_ && constrs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [deriving]
  private static boolean itdecl_1_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "itdecl_1_3")) return false;
    deriving(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // bkind ['->' kind]
  public static boolean kind(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "kind")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<kind>");
    result_ = bkind(builder_, level_ + 1);
    result_ = result_ && kind_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, KIND, result_, false, null);
    return result_;
  }

  // ['->' kind]
  private static boolean kind_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "kind_1")) return false;
    kind_1_0(builder_, level_ + 1);
    return true;
  }

  // '->' kind
  private static boolean kind_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "kind_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, RIGHTARROW);
    result_ = result_ && kind(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // "::" kind
  static boolean kindsig(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "kindsig")) return false;
    if (!nextTokenIs(builder_, DOUBLECOLON)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, DOUBLECOLON);
    pinned_ = result_; // pin = 1
    result_ = result_ && kind(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // "\\case" altslist
  //                | '\' apat+ "->" exp
  //                | "let" decls "in" exp
  //                | "if" exp [semi] "then" exp [semi] "else" exp
  //                | "case" exp "of" altslist
  //                | "do" open [stmts] close
  //                | "mdo" open [stmts] close
  //                | "proc" aexp "->" exp
  //                | ppragma exp
  //                | fexp
  static boolean lexp(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = lexp_0(builder_, level_ + 1);
    if (!result_) result_ = lexp_1(builder_, level_ + 1);
    if (!result_) result_ = lexp_2(builder_, level_ + 1);
    if (!result_) result_ = lexp_3(builder_, level_ + 1);
    if (!result_) result_ = lexp_4(builder_, level_ + 1);
    if (!result_) result_ = lexp_5(builder_, level_ + 1);
    if (!result_) result_ = lexp_6(builder_, level_ + 1);
    if (!result_) result_ = lexp_7(builder_, level_ + 1);
    if (!result_) result_ = lexp_8(builder_, level_ + 1);
    if (!result_) result_ = fexp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "\\case" altslist
  private static boolean lexp_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "\\case");
    result_ = result_ && altslist(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '\' apat+ "->" exp
  private static boolean lexp_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKSLASH);
    result_ = result_ && lexp_1_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RIGHTARROW);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // apat+
  private static boolean lexp_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!apat(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "lexp_1_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "let" decls "in" exp
  private static boolean lexp_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LET);
    result_ = result_ && decls(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, IN);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "if" exp [semi] "then" exp [semi] "else" exp
  private static boolean lexp_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, IF);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && lexp_3_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, THEN);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && lexp_3_5(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ELSE);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [semi]
  private static boolean lexp_3_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_3_2")) return false;
    semi(builder_, level_ + 1);
    return true;
  }

  // [semi]
  private static boolean lexp_3_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_3_5")) return false;
    semi(builder_, level_ + 1);
    return true;
  }

  // "case" exp "of" altslist
  private static boolean lexp_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CASE);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, OF);
    result_ = result_ && altslist(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "do" open [stmts] close
  private static boolean lexp_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_5")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DO);
    result_ = result_ && open(builder_, level_ + 1);
    result_ = result_ && lexp_5_2(builder_, level_ + 1);
    result_ = result_ && close(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [stmts]
  private static boolean lexp_5_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_5_2")) return false;
    stmts(builder_, level_ + 1);
    return true;
  }

  // "mdo" open [stmts] close
  private static boolean lexp_6(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_6")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, MDOTOK);
    result_ = result_ && open(builder_, level_ + 1);
    result_ = result_ && lexp_6_2(builder_, level_ + 1);
    result_ = result_ && close(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [stmts]
  private static boolean lexp_6_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_6_2")) return false;
    stmts(builder_, level_ + 1);
    return true;
  }

  // "proc" aexp "->" exp
  private static boolean lexp_7(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_7")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "proc");
    result_ = result_ && aexp(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RIGHTARROW);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ppragma exp
  private static boolean lexp_8(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexp_8")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ppragma(builder_, level_ + 1);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '[' exp [listlike1] ']'
  static boolean listlike(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike")) return false;
    if (!nextTokenIs(builder_, LBRACKET)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, LBRACKET);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, exp(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, listlike_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, RBRACKET) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [listlike1]
  private static boolean listlike_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike_2")) return false;
    listlike1(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // ('|' (squal ',')* squal)+
  //                     | [',' exp] '..' [exp]
  //                     | (',' exp)+
  static boolean listlike1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = listlike1_0(builder_, level_ + 1);
    if (!result_) result_ = listlike1_1(builder_, level_ + 1);
    if (!result_) result_ = listlike1_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ('|' (squal ',')* squal)+
  private static boolean listlike1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = listlike1_0_0(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!listlike1_0_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "listlike1_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '|' (squal ',')* squal
  private static boolean listlike1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, PIPE);
    result_ = result_ && listlike1_0_0_1(builder_, level_ + 1);
    result_ = result_ && squal(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (squal ',')*
  private static boolean listlike1_0_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_0_0_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!listlike1_0_0_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "listlike1_0_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // squal ','
  private static boolean listlike1_0_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_0_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = squal(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [',' exp] '..' [exp]
  private static boolean listlike1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = listlike1_1_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEPERIOD);
    result_ = result_ && listlike1_1_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [',' exp]
  private static boolean listlike1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_1_0")) return false;
    listlike1_1_0_0(builder_, level_ + 1);
    return true;
  }

  // ',' exp
  private static boolean listlike1_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [exp]
  private static boolean listlike1_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_1_2")) return false;
    exp(builder_, level_ + 1);
    return true;
  }

  // (',' exp)+
  private static boolean listlike1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = listlike1_2_0(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!listlike1_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "listlike1_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ',' exp
  private static boolean listlike1_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "listlike1_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // floattoken ['#'|'##'] | integertoken ['#'|'##'] | chartoken ['#'] | pstringtoken ['#']
  static boolean literal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = literal_0(builder_, level_ + 1);
    if (!result_) result_ = literal_1(builder_, level_ + 1);
    if (!result_) result_ = literal_2(builder_, level_ + 1);
    if (!result_) result_ = literal_3(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // floattoken ['#'|'##']
  private static boolean literal_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, FLOATTOKEN);
    result_ = result_ && literal_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['#'|'##']
  private static boolean literal_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_0_1")) return false;
    literal_0_1_0(builder_, level_ + 1);
    return true;
  }

  // '#'|'##'
  private static boolean literal_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, HASH);
    if (!result_) result_ = consumeToken(builder_, DOUBLEHASH);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // integertoken ['#'|'##']
  private static boolean literal_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, INTEGERTOKEN);
    result_ = result_ && literal_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['#'|'##']
  private static boolean literal_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_1_1")) return false;
    literal_1_1_0(builder_, level_ + 1);
    return true;
  }

  // '#'|'##'
  private static boolean literal_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, HASH);
    if (!result_) result_ = consumeToken(builder_, DOUBLEHASH);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // chartoken ['#']
  private static boolean literal_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CHARTOKEN);
    result_ = result_ && literal_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['#']
  private static boolean literal_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_2_1")) return false;
    consumeToken(builder_, HASH);
    return true;
  }

  // pstringtoken ['#']
  private static boolean literal_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pstringtoken(builder_, level_ + 1);
    result_ = result_ && literal_3_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['#']
  private static boolean literal_3_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal_3_1")) return false;
    consumeToken(builder_, HASH);
    return true;
  }

  /* ********************************************************** */
  // '-' (integertoken|floattoken)
  //                | gcon apat+
  //                | apat
  static boolean lpat(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lpat")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = lpat_0(builder_, level_ + 1);
    if (!result_) result_ = lpat_1(builder_, level_ + 1);
    if (!result_) result_ = apat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '-' (integertoken|floattoken)
  private static boolean lpat_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lpat_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, MINUS);
    result_ = result_ && lpat_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // integertoken|floattoken
  private static boolean lpat_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lpat_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, INTEGERTOKEN);
    if (!result_) result_ = consumeToken(builder_, FLOATTOKEN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // gcon apat+
  private static boolean lpat_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lpat_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = gcon(builder_, level_ + 1);
    result_ = result_ && lpat_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // apat+
  private static boolean lpat_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lpat_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = apat(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!apat(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "lpat_1_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // (ppragma | cpp)* [moduledecl] body
  static boolean module(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_0(builder_, level_ + 1);
    result_ = result_ && module_1(builder_, level_ + 1);
    result_ = result_ && body(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (ppragma | cpp)*
  private static boolean module_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!module_0_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "module_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ppragma | cpp
  private static boolean module_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ppragma(builder_, level_ + 1);
    if (!result_) result_ = cpp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [moduledecl]
  private static boolean module_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_1")) return false;
    moduledecl(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // (conidRegexp '.')+
  public static boolean modulePrefix(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modulePrefix")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
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

  // conidRegexp '.'
  private static boolean modulePrefix_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modulePrefix_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONIDREGEXP);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // moduleline
  public static boolean moduledecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "moduledecl")) return false;
    if (!nextTokenIs(builder_, MODULETOKEN)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = moduleline(builder_, level_ + 1);
    exit_section_(builder_, marker_, MODULEDECL, result_);
    return result_;
  }

  /* ********************************************************** */
  // "module" qconid [ppragma] [exports] "where"
  static boolean moduleline(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "moduleline")) return false;
    if (!nextTokenIs(builder_, MODULETOKEN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, MODULETOKEN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, qconid(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, moduleline_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, moduleline_3(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, WHERE) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [ppragma]
  private static boolean moduleline_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "moduleline_2")) return false;
    ppragma(builder_, level_ + 1);
    return true;
  }

  // [exports]
  private static boolean moduleline_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "moduleline_3")) return false;
    exports(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // con atype
  //             | con '{' var '::' typee '}'
  public static boolean newconstr(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newconstr")) return false;
    if (!nextTokenIs(builder_, "<newconstr>", LPAREN, CONIDREGEXP)) return false;
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
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = con(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LBRACE);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, var(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, DOUBLECOLON)) && result_;
    result_ = pinned_ && report_error_(builder_, typee(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, RBRACE) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // EOL
  static boolean newline(PsiBuilder builder_, int level_) {
    return consumeToken(builder_, EOL);
  }

  /* ********************************************************** */
  // "newtype" [context "=>"] simpletype '=' newconstr [deriving]
  public static boolean newtypedecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newtypedecl")) return false;
    if (!nextTokenIs(builder_, NEWTYPE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, NEWTYPE);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, newtypedecl_1(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, simpletype(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, EQUALS)) && result_;
    result_ = pinned_ && report_error_(builder_, newconstr(builder_, level_ + 1)) && result_;
    result_ = pinned_ && newtypedecl_5(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, NEWTYPEDECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [context "=>"]
  private static boolean newtypedecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newtypedecl_1")) return false;
    newtypedecl_1_0(builder_, level_ + 1);
    return true;
  }

  // context "=>"
  private static boolean newtypedecl_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newtypedecl_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = context(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [deriving]
  private static boolean newtypedecl_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newtypedecl_5")) return false;
    deriving(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '(#' commas '#)'
  //                    | '(' ('->' | commas) ')'
  // //                   | '[:' ':]'
  //                    | '[' ']'
  // //                   | '(' '~#' ')'
  //                    | oqtycon
  static boolean ntgtycon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ntgtycon")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ntgtycon_0(builder_, level_ + 1);
    if (!result_) result_ = ntgtycon_1(builder_, level_ + 1);
    if (!result_) result_ = ntgtycon_2(builder_, level_ + 1);
    if (!result_) result_ = oqtycon(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(#' commas '#)'
  private static boolean ntgtycon_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ntgtycon_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LUNBOXPAREN);
    result_ = result_ && commas(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RUNBOXPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' ('->' | commas) ')'
  private static boolean ntgtycon_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ntgtycon_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && ntgtycon_1_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '->' | commas
  private static boolean ntgtycon_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ntgtycon_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, RIGHTARROW);
    if (!result_) result_ = commas(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' ']'
  private static boolean ntgtycon_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ntgtycon_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && consumeToken(builder_, RBRACKET);
    exit_section_(builder_, marker_, null, result_);
    return result_;
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
  // '{'
  //                 | WHITESPACELBRACETOK
  static boolean open(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "open")) return false;
    if (!nextTokenIs(builder_, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACE);
    if (!result_) result_ = consumeToken(builder_, WHITESPACELBRACETOK);
    exit_section_(builder_, marker_, null, result_);
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
  // '(' '~' ')'         // An "ordinary" qualified tycon;
  //           | '(' qtyconsym ')'  // These can appear in export lists
  //           | qtycon
  public static boolean oqtycon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "oqtycon")) return false;
    if (!nextTokenIs(builder_, "<oqtycon>", LPAREN, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<oqtycon>");
    result_ = oqtycon_0(builder_, level_ + 1);
    if (!result_) result_ = oqtycon_1(builder_, level_ + 1);
    if (!result_) result_ = qtycon(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, OQTYCON, result_, false, null);
    return result_;
  }

  // '(' '~' ')'
  private static boolean oqtycon_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "oqtycon_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && consumeToken(builder_, TILDE);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' qtyconsym ')'
  private static boolean oqtycon_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "oqtycon_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && qtyconsym(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '(' parenlike1
  static boolean parenlike(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike")) return false;
    if (!nextTokenIs(builder_, LPAREN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, LPAREN);
    pinned_ = result_; // pin = 1
    result_ = result_ && parenlike1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // exp [parenlikeoptseq] ')'
  //                      | parenlike2 ')'
  static boolean parenlike1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = parenlike1_0(builder_, level_ + 1);
    if (!result_) result_ = parenlike1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // exp [parenlikeoptseq] ')'
  private static boolean parenlike1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = exp(builder_, level_ + 1);
    result_ = result_ && parenlike1_0_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [parenlikeoptseq]
  private static boolean parenlike1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike1_0_1")) return false;
    parenlikeoptseq(builder_, level_ + 1);
    return true;
  }

  // parenlike2 ')'
  private static boolean parenlike1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = parenlike2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // infixexp qop
  //                      | qop infixexp
  static boolean parenlike2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = parenlike2_0(builder_, level_ + 1);
    if (!result_) result_ = parenlike2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // infixexp qop
  private static boolean parenlike2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = infixexp(builder_, level_ + 1);
    result_ = result_ && qop(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qop infixexp
  private static boolean parenlike2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlike2_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qop(builder_, level_ + 1);
    result_ = result_ && infixexp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ',' (exp ',')* exp
  static boolean parenlikeoptseq(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlikeoptseq")) return false;
    if (!nextTokenIs(builder_, COMMA)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, parenlikeoptseq_1(builder_, level_ + 1));
    result_ = pinned_ && exp(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (exp ',')*
  private static boolean parenlikeoptseq_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlikeoptseq_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!parenlikeoptseq_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "parenlikeoptseq_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // exp ','
  private static boolean parenlikeoptseq_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenlikeoptseq_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = exp(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // lpat (qconop pat | ["::" ctype])
  public static boolean pat(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pat")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<pat>");
    result_ = lpat(builder_, level_ + 1);
    result_ = result_ && pat_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, PAT, result_, false, null);
    return result_;
  }

  // qconop pat | ["::" ctype]
  private static boolean pat_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pat_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pat_1_0(builder_, level_ + 1);
    if (!result_) result_ = pat_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qconop pat
  private static boolean pat_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pat_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qconop(builder_, level_ + 1);
    result_ = result_ && pat(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["::" ctype]
  private static boolean pat_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pat_1_1")) return false;
    pat_1_1_0(builder_, level_ + 1);
    return true;
  }

  // "::" ctype
  private static boolean pat_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pat_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOUBLECOLON);
    result_ = result_ && ctype(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // qtycon
  //                 | '(' [kind ',' comma_kinds1] ')'
  //                 | '[' kind [',' comma_kinds1] ']'
  static boolean pkind(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pkind")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycon(builder_, level_ + 1);
    if (!result_) result_ = pkind_1(builder_, level_ + 1);
    if (!result_) result_ = pkind_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' [kind ',' comma_kinds1] ')'
  private static boolean pkind_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pkind_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && pkind_1_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [kind ',' comma_kinds1]
  private static boolean pkind_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pkind_1_1")) return false;
    pkind_1_1_0(builder_, level_ + 1);
    return true;
  }

  // kind ',' comma_kinds1
  private static boolean pkind_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pkind_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = kind(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, COMMA);
    result_ = result_ && comma_kinds1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '[' kind [',' comma_kinds1] ']'
  private static boolean pkind_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pkind_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LBRACKET);
    result_ = result_ && kind(builder_, level_ + 1);
    result_ = result_ && pkind_2_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RBRACKET);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [',' comma_kinds1]
  private static boolean pkind_2_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pkind_2_2")) return false;
    pkind_2_2_0(builder_, level_ + 1);
    return true;
  }

  // ',' comma_kinds1
  private static boolean pkind_2_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pkind_2_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COMMA);
    result_ = result_ && comma_kinds1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // openpragma PRAGMA+ closepragma
  public static boolean ppragma(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ppragma")) return false;
    if (!nextTokenIs(builder_, OPENPRAGMA)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OPENPRAGMA);
    result_ = result_ && ppragma_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CLOSEPRAGMA);
    exit_section_(builder_, marker_, PPRAGMA, result_);
    return result_;
  }

  // PRAGMA+
  private static boolean ppragma_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ppragma_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, PRAGMA);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!consumeToken(builder_, PRAGMA)) break;
      if (!empty_element_parsed_guard_(builder_, "ppragma_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '"' STRINGTOKEN* '"'
  public static boolean pstringtoken(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pstringtoken")) return false;
    if (!nextTokenIs(builder_, DOUBLEQUOTE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOUBLEQUOTE);
    result_ = result_ && pstringtoken_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEQUOTE);
    exit_section_(builder_, marker_, PSTRINGTOKEN, result_);
    return result_;
  }

  // STRINGTOKEN*
  private static boolean pstringtoken_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "pstringtoken_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consumeToken(builder_, STRINGTOKEN)) break;
      if (!empty_element_parsed_guard_(builder_, "pstringtoken_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // qconid | '(' gconsym ')'
  public static boolean qcon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qcon")) return false;
    if (!nextTokenIs(builder_, "<qcon>", LPAREN, CONIDREGEXP)) return false;
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
  // [modulePrefix] conidRegexp
  public static boolean qconid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconid")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qconid_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CONIDREGEXP);
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
    if (!nextTokenIs(builder_, "<qconsym>", CONSYMTOK, CONIDREGEXP)) return false;
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
  // '`' [modulePrefix] conidRegexp '`'
  public static boolean qinfixconid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qinfixconid")) return false;
    if (!nextTokenIs(builder_, BACKTICK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && qinfixconid_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CONIDREGEXP);
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
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
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
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
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
  // qtyconsym | '`' qtycon '`'
  public static boolean qtyconop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qtyconop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qtyconop>");
    result_ = qtyconsym(builder_, level_ + 1);
    if (!result_) result_ = qtyconop_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QTYCONOP, result_, false, null);
    return result_;
  }

  // '`' qtycon '`'
  private static boolean qtyconop_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qtyconop_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKTICK);
    result_ = result_ && qtycon(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, BACKTICK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // qconsym | qvarsym | tyconsym
  public static boolean qtyconsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qtyconsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qtyconsym>");
    result_ = qconsym(builder_, level_ + 1);
    if (!result_) result_ = qvarsym(builder_, level_ + 1);
    if (!result_) result_ = tyconsym(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QTYCONSYM, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // pat '<-' exp
  //               | "let" decls
  //               | exp
  static boolean qual(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qual")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qual_0(builder_, level_ + 1);
    if (!result_) result_ = qual_1(builder_, level_ + 1);
    if (!result_) result_ = exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // pat '<-' exp
  private static boolean qual_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qual_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = pat(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LEFTARROW);
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // "let" decls
  private static boolean qual_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qual_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LET);
    result_ = result_ && decls(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
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
  // qvar
  //                         | qcon
  //                         | gcon
  //                         | '(' recordlikeparen ')'
  static boolean recordlikelhs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "recordlikelhs")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qvar(builder_, level_ + 1);
    if (!result_) result_ = qcon(builder_, level_ + 1);
    if (!result_) result_ = gcon(builder_, level_ + 1);
    if (!result_) result_ = recordlikelhs_3(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' recordlikeparen ')'
  private static boolean recordlikelhs_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "recordlikelhs_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && recordlikeparen(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // exp
  //                           | infixexp qop
  //                           | qop infixexp
  static boolean recordlikeparen(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "recordlikeparen")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = exp(builder_, level_ + 1);
    if (!result_) result_ = recordlikeparen_1(builder_, level_ + 1);
    if (!result_) result_ = recordlikeparen_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // infixexp qop
  private static boolean recordlikeparen_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "recordlikeparen_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = infixexp(builder_, level_ + 1);
    result_ = result_ && qop(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qop infixexp
  private static boolean recordlikeparen_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "recordlikeparen_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qop(builder_, level_ + 1);
    result_ = result_ && infixexp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // 'class' | 'data' | 'default' | 'deriving' | 'foreign' | 'instance'
  //                        | 'newtype' | 'type' | 'where' | 'forall'
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
    if (!result_) result_ = consumeToken(builder_, FORALLTOKEN);
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
  // 'export' | 'foreign' | 'import' | 'infix'
  //                        | 'infixl' | 'infixr'
  static boolean reservedMeta(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedMeta")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, EXPORTTOKEN);
    if (!result_) result_ = consumeToken(builder_, FOREIGN);
    if (!result_) result_ = consumeToken(builder_, IMPORT);
    if (!result_) result_ = consumeToken(builder_, INFIX);
    if (!result_) result_ = consumeToken(builder_, INFIXL);
    if (!result_) result_ = consumeToken(builder_, INFIXR);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '_'
  static boolean reservedVar(PsiBuilder builder_, int level_) {
    return consumeToken(builder_, UNDERSCORE);
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
  // ('=' exp | gdrhs+) [wheredecls]
  public static boolean rhs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rhs")) return false;
    if (!nextTokenIs(builder_, "<rhs>", EQUALS, PIPE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<rhs>");
    result_ = rhs_0(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && rhs_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, RHS, result_, pinned_, null);
    return result_ || pinned_;
  }

  // '=' exp | gdrhs+
  private static boolean rhs_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rhs_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = rhs_0_0(builder_, level_ + 1);
    if (!result_) result_ = rhs_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '=' exp
  private static boolean rhs_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rhs_0_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, EQUALS);
    pinned_ = result_; // pin = 1
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // gdrhs+
  private static boolean rhs_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rhs_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = gdrhs(builder_, level_ + 1);
    int pos_ = current_position_(builder_);
    while (result_) {
      if (!gdrhs(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "rhs_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [wheredecls]
  private static boolean rhs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rhs_1")) return false;
    wheredecls(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // "unsafe" | "safe" | "interruptible"
  static boolean safety(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "safety")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "unsafe");
    if (!result_) result_ = consumeToken(builder_, "safe");
    if (!result_) result_ = consumeToken(builder_, "interruptible");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ';'
  //                | WHITESPACESEMITOK
  static boolean semi(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "semi")) return false;
    if (!nextTokenIs(builder_, "", SEMICOLON, WHITESPACESEMITOK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, SEMICOLON);
    if (!result_) result_ = consumeToken(builder_, WHITESPACESEMITOK);
    exit_section_(builder_, marker_, null, result_);
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
  // tycon tyvar*
  static boolean simpletype(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "simpletype")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
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
  // transformqual
  //                 | qual
  static boolean squal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "squal")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = transformqual(builder_, level_ + 1);
    if (!result_) result_ = qual(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [exp] semi
  //                | pat '<-' exp semi
  //                | ["rec"] "let" decls semi
  static boolean stmt(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmt")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = stmt_0(builder_, level_ + 1);
    if (!result_) result_ = stmt_1(builder_, level_ + 1);
    if (!result_) result_ = stmt_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [exp] semi
  private static boolean stmt_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmt_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = stmt_0_0(builder_, level_ + 1);
    result_ = result_ && semi(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [exp]
  private static boolean stmt_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmt_0_0")) return false;
    exp(builder_, level_ + 1);
    return true;
  }

  // pat '<-' exp semi
  private static boolean stmt_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmt_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = pat(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LEFTARROW);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, exp(builder_, level_ + 1));
    result_ = pinned_ && semi(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["rec"] "let" decls semi
  private static boolean stmt_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmt_2")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = stmt_2_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, LET);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, decls(builder_, level_ + 1));
    result_ = pinned_ && semi(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ["rec"]
  private static boolean stmt_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmt_2_0")) return false;
    consumeToken(builder_, RECTOK);
    return true;
  }

  /* ********************************************************** */
  // stmt* exp [semi]
  public static boolean stmts(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmts")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<stmts>");
    result_ = stmts_0(builder_, level_ + 1);
    result_ = result_ && exp(builder_, level_ + 1);
    result_ = result_ && stmts_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, STMTS, result_, false, null);
    return result_;
  }

  // stmt*
  private static boolean stmts_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmts_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!stmt(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "stmts_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // [semi]
  private static boolean stmts_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stmts_2")) return false;
    semi(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // symbol1 | '=' | '|' | '!' | '#' |
  public static boolean symbol(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "symbol")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<symbol>");
    result_ = symbol1(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, EQUALS);
    if (!result_) result_ = consumeToken(builder_, PIPE);
    if (!result_) result_ = consumeToken(builder_, EXCLAMATION);
    if (!result_) result_ = consumeToken(builder_, HASH);
    if (!result_) result_ = consumeToken(builder_, SYMBOL_5_0);
    exit_section_(builder_, level_, marker_, SYMBOL, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '>' | '?' | '@'
  //          | '\' | '^' | '-' | '~' | ':'
  static boolean symbol1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "symbol1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOLLAR);
    if (!result_) result_ = consumeToken(builder_, PERCENT);
    if (!result_) result_ = consumeToken(builder_, AMPERSAND);
    if (!result_) result_ = consumeToken(builder_, ASTERISK);
    if (!result_) result_ = consumeToken(builder_, PLUS);
    if (!result_) result_ = consumeToken(builder_, PERIOD);
    if (!result_) result_ = consumeToken(builder_, SLASH);
    if (!result_) result_ = consumeToken(builder_, LESSTHAN);
    if (!result_) result_ = consumeToken(builder_, GREATERTHAN);
    if (!result_) result_ = consumeToken(builder_, QUESTION);
    if (!result_) result_ = consumeToken(builder_, AMPERSAT);
    if (!result_) result_ = consumeToken(builder_, BACKSLASH);
    if (!result_) result_ = consumeToken(builder_, CARET);
    if (!result_) result_ = consumeToken(builder_, MINUS);
    if (!result_) result_ = consumeToken(builder_, TILDE);
    if (!result_) result_ = consumeToken(builder_, COLON);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // topdecl1 ppragma*
  static boolean topdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = topdecl1(builder_, level_ + 1);
    result_ = result_ && topdecl_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ppragma*
  private static boolean topdecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!ppragma(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "topdecl_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // typedecl
  //                   | datadecl
  //                   | newtypedecl
  //                   | classdecl
  //                   | instancedecl
  //                   | defaultdecl
  //                   | foreigndecl
  //                   | derivingdecl
  //                   | decl
  //                   | infixexp
  static boolean topdecl1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = typedecl(builder_, level_ + 1);
    if (!result_) result_ = datadecl(builder_, level_ + 1);
    if (!result_) result_ = newtypedecl(builder_, level_ + 1);
    if (!result_) result_ = classdecl(builder_, level_ + 1);
    if (!result_) result_ = instancedecl(builder_, level_ + 1);
    if (!result_) result_ = defaultdecl(builder_, level_ + 1);
    if (!result_) result_ = foreigndecl(builder_, level_ + 1);
    if (!result_) result_ = derivingdecl(builder_, level_ + 1);
    if (!result_) result_ = decl(builder_, level_ + 1);
    if (!result_) result_ = infixexp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // topdecl [semi topdecls]
  static boolean topdecls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecls")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = topdecl(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && topdecls_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [semi topdecls]
  private static boolean topdecls_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecls_1")) return false;
    topdecls_1_0(builder_, level_ + 1);
    return true;
  }

  // semi topdecls
  private static boolean topdecls_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecls_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = semi(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && topdecls(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'then' ('group' ['by' exp] 'using' exp | exp ['by' exp])
  static boolean transformqual(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual")) return false;
    if (!nextTokenIs(builder_, THEN)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, THEN);
    result_ = result_ && transformqual_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // 'group' ['by' exp] 'using' exp | exp ['by' exp]
  private static boolean transformqual_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = transformqual_1_0(builder_, level_ + 1);
    if (!result_) result_ = transformqual_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // 'group' ['by' exp] 'using' exp
  private static boolean transformqual_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "group");
    result_ = result_ && transformqual_1_0_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, "using");
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['by' exp]
  private static boolean transformqual_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual_1_0_1")) return false;
    transformqual_1_0_1_0(builder_, level_ + 1);
    return true;
  }

  // 'by' exp
  private static boolean transformqual_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "by");
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // exp ['by' exp]
  private static boolean transformqual_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = exp(builder_, level_ + 1);
    result_ = result_ && transformqual_1_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['by' exp]
  private static boolean transformqual_1_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual_1_1_1")) return false;
    transformqual_1_1_1_0(builder_, level_ + 1);
    return true;
  }

  // 'by' exp
  private static boolean transformqual_1_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "transformqual_1_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "by");
    result_ = result_ && exp(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '(' tyvar kindsig ')'
  //           | tyvar
  public static boolean tv_bndr(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tv_bndr")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<tv bndr>");
    result_ = tv_bndr_0(builder_, level_ + 1);
    if (!result_) result_ = tyvar(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, TV_BNDR, result_, false, null);
    return result_;
  }

  // '(' tyvar kindsig ')'
  private static boolean tv_bndr_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tv_bndr_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, LPAREN);
    result_ = result_ && tyvar(builder_, level_ + 1);
    result_ = result_ && kindsig(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, RPAREN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // conid
  public static boolean tycls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tycls")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = conid(builder_, level_ + 1);
    exit_section_(builder_, marker_, TYCLS, result_);
    return result_;
  }

  /* ********************************************************** */
  // conid
  public static boolean tycon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tycon")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = conid(builder_, level_ + 1);
    exit_section_(builder_, marker_, TYCON, result_);
    return result_;
  }

  /* ********************************************************** */
  // consym | varsym | '*' | '-'
  public static boolean tyconsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tyconsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<tyconsym>");
    result_ = consym(builder_, level_ + 1);
    if (!result_) result_ = varsym(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, ASTERISK);
    if (!result_) result_ = consumeToken(builder_, MINUS);
    exit_section_(builder_, level_, marker_, TYCONSYM, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // "type" typee '=' typee
  public static boolean typedecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typedecl")) return false;
    if (!nextTokenIs(builder_, TYPE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, TYPE);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, typee(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, EQUALS)) && result_;
    result_ = pinned_ && typee(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, TYPEDECL, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // btype typeeaux
  public static boolean typee(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typee")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<typee>");
    result_ = btype(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && typeeaux(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, TYPEE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // (singlequote (qconop | varop) | qtyconop) typee
  //                    | [typeeopt]
  static boolean typeeaux(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typeeaux")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = typeeaux_0(builder_, level_ + 1);
    if (!result_) result_ = typeeaux_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // (singlequote (qconop | varop) | qtyconop) typee
  private static boolean typeeaux_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typeeaux_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = typeeaux_0_0(builder_, level_ + 1);
    result_ = result_ && typee(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // singlequote (qconop | varop) | qtyconop
  private static boolean typeeaux_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typeeaux_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = typeeaux_0_0_0(builder_, level_ + 1);
    if (!result_) result_ = qtyconop(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // singlequote (qconop | varop)
  private static boolean typeeaux_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typeeaux_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, SINGLEQUOTE);
    result_ = result_ && typeeaux_0_0_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qconop | varop
  private static boolean typeeaux_0_0_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typeeaux_0_0_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qconop(builder_, level_ + 1);
    if (!result_) result_ = varop(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [typeeopt]
  private static boolean typeeaux_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typeeaux_1")) return false;
    typeeopt(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // "->" typee
  static boolean typeeopt(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typeeopt")) return false;
    if (!nextTokenIs(builder_, RIGHTARROW)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, RIGHTARROW);
    pinned_ = result_; // pin = 1
    result_ = result_ && typee(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // varid
  public static boolean tyvar(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tyvar")) return false;
    if (!nextTokenIs(builder_, "<tyvar>", AS, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<tyvar>");
    result_ = varid(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, TYVAR, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // varid | '(' varsym ')'
  static boolean var(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "var")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = varid(builder_, level_ + 1);
    if (!result_) result_ = var_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
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
  // !reservedid (varidRegexp | "as")
  public static boolean varid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid")) return false;
    if (!nextTokenIs(builder_, "<varid>", AS, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<varid>");
    result_ = varid_0(builder_, level_ + 1);
    result_ = result_ && varid_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, VARID, result_, false, null);
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

  // varidRegexp | "as"
  private static boolean varid_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, VARIDREGEXP);
    if (!result_) result_ = consumeToken(builder_, AS);
    exit_section_(builder_, marker_, null, result_);
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
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<vars>");
    result_ = sequence(builder_, level_ + 1, var_parser_);
    exit_section_(builder_, level_, marker_, VARS, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // VARSYMTOK | symbol1
  public static boolean varsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<varsym>");
    result_ = consumeToken(builder_, VARSYMTOK);
    if (!result_) result_ = symbol1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, VARSYM, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // "where" decls
  static boolean wheredecls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "wheredecls")) return false;
    if (!nextTokenIs(builder_, WHERE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, WHERE);
    pinned_ = result_; // pin = 1
    result_ = result_ && decls(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
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
  // whitechar | comment | haddock
  static boolean whitestuff(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitestuff")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitechar(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, COMMENT);
    if (!result_) result_ = consumeToken(builder_, HADDOCK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  final static Parser cname_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return cname(builder_, level_ + 1);
    }
  };
  final static Parser con_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return con(builder_, level_ + 1);
    }
  };
  final static Parser ctype_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return ctype(builder_, level_ + 1);
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
