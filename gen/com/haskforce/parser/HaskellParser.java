// This is a generated file. Not intended for manual editing.
package com.haskforce.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static com.haskforce.psi.HaskellTypes.*;
import static com.haskforce.psi.HaskellParserUtilBase.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class HaskellParser implements PsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, null);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    if (t == AKIND) {
      r = akind(b, 0);
    }
    else if (t == ALT) {
      r = alt(b, 0);
    }
    else if (t == ATYPE) {
      r = atype(b, 0);
    }
    else if (t == BKIND) {
      r = bkind(b, 0);
    }
    else if (t == BODY) {
      r = body(b, 0);
    }
    else if (t == CDECL) {
      r = cdecl(b, 0);
    }
    else if (t == CLASSDECL) {
      r = classdecl(b, 0);
    }
    else if (t == CON) {
      r = con(b, 0);
    }
    else if (t == CONID) {
      r = conid(b, 0);
    }
    else if (t == CONOP) {
      r = conop(b, 0);
    }
    else if (t == CONSTR) {
      r = constr(b, 0);
    }
    else if (t == CONSYM) {
      r = consym(b, 0);
    }
    else if (t == CONTEXT) {
      r = context(b, 0);
    }
    else if (t == CTYPE) {
      r = ctype(b, 0);
    }
    else if (t == DATADECL) {
      r = datadecl(b, 0);
    }
    else if (t == DEFAULTDECL) {
      r = defaultdecl(b, 0);
    }
    else if (t == DERIVINGDECL) {
      r = derivingdecl(b, 0);
    }
    else if (t == EXP) {
      r = exp(b, 0);
    }
    else if (t == EXPORT) {
      r = export(b, 0);
    }
    else if (t == EXPORTS) {
      r = exports(b, 0);
    }
    else if (t == EXPORTSEMPTY) {
      r = exportsempty(b, 0);
    }
    else if (t == FIXITY) {
      r = fixity(b, 0);
    }
    else if (t == FOREIGNDECL) {
      r = foreigndecl(b, 0);
    }
    else if (t == FUNORPATDECL) {
      r = funorpatdecl(b, 0);
    }
    else if (t == GCONSYM) {
      r = gconsym(b, 0);
    }
    else if (t == GENDECL) {
      r = gendecl(b, 0);
    }
    else if (t == GUARD) {
      r = guard(b, 0);
    }
    else if (t == IDECL) {
      r = idecl(b, 0);
    }
    else if (t == IMPDECL) {
      r = impdecl(b, 0);
    }
    else if (t == IMPEMPTY) {
      r = impempty(b, 0);
    }
    else if (t == IMPORTT) {
      r = importt(b, 0);
    }
    else if (t == INSTANCEDECL) {
      r = instancedecl(b, 0);
    }
    else if (t == KIND) {
      r = kind(b, 0);
    }
    else if (t == LETEXP) {
      r = letexp(b, 0);
    }
    else if (t == MODULEDECL) {
      r = moduledecl(b, 0);
    }
    else if (t == NEWCONSTR) {
      r = newconstr(b, 0);
    }
    else if (t == NEWTYPEDECL) {
      r = newtypedecl(b, 0);
    }
    else if (t == OP) {
      r = op(b, 0);
    }
    else if (t == OQTYCON) {
      r = oqtycon(b, 0);
    }
    else if (t == PAT) {
      r = pat(b, 0);
    }
    else if (t == PPRAGMA) {
      r = ppragma(b, 0);
    }
    else if (t == PSTRINGTOKEN) {
      r = pstringtoken(b, 0);
    }
    else if (t == QCON) {
      r = qcon(b, 0);
    }
    else if (t == QCONID) {
      r = qconid(b, 0);
    }
    else if (t == QCONOP) {
      r = qconop(b, 0);
    }
    else if (t == QCONSYM) {
      r = qconsym(b, 0);
    }
    else if (t == QOP) {
      r = qop(b, 0);
    }
    else if (t == QQBLOB) {
      r = qqblob(b, 0);
    }
    else if (t == QTYCLS) {
      r = qtycls(b, 0);
    }
    else if (t == QTYCON) {
      r = qtycon(b, 0);
    }
    else if (t == QTYCONOP) {
      r = qtyconop(b, 0);
    }
    else if (t == QTYCONSYM) {
      r = qtyconsym(b, 0);
    }
    else if (t == QVAR) {
      r = qvar(b, 0);
    }
    else if (t == QVARID) {
      r = qvarid(b, 0);
    }
    else if (t == QVAROP) {
      r = qvarop(b, 0);
    }
    else if (t == QVARS) {
      r = qvars(b, 0);
    }
    else if (t == QVARSYM) {
      r = qvarsym(b, 0);
    }
    else if (t == RHS) {
      r = rhs(b, 0);
    }
    else if (t == SHEBANG) {
      r = shebang(b, 0);
    }
    else if (t == STMTS) {
      r = stmts(b, 0);
    }
    else if (t == TV_BNDR) {
      r = tv_bndr(b, 0);
    }
    else if (t == TYCLS) {
      r = tycls(b, 0);
    }
    else if (t == TYCON) {
      r = tycon(b, 0);
    }
    else if (t == TYCONSYM) {
      r = tyconsym(b, 0);
    }
    else if (t == TYPEDECL) {
      r = typedecl(b, 0);
    }
    else if (t == TYPEE) {
      r = typee(b, 0);
    }
    else if (t == TYVAR) {
      r = tyvar(b, 0);
    }
    else if (t == VARID) {
      r = varid(b, 0);
    }
    else if (t == VAROP) {
      r = varop(b, 0);
    }
    else if (t == VARS) {
      r = vars(b, 0);
    }
    else if (t == VARSYM) {
      r = varsym(b, 0);
    }
    else {
      r = parse_root_(t, b, 0);
    }
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return module(b, l + 1);
  }

  /* ********************************************************** */
  // parensplice exp i ')'
  //                | singlequote i (qvar | qcon)
  //                | idsplice
  //                | thquote i qcon
  //                | literal
  //                | thaexp
  //                | '(#' i '#)'
  //                | listlike
  //                | parenlike
  //                | [recordlikelhs] i '{' (fbind ',')* [e] (".." | fbind) [e] '}'
  //                | gcon
  //                | qvar
  static boolean aexp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = aexp_0(b, l + 1);
    if (!r) r = aexp_1(b, l + 1);
    if (!r) r = consumeToken(b, IDSPLICE);
    if (!r) r = aexp_3(b, l + 1);
    if (!r) r = literal(b, l + 1);
    if (!r) r = thaexp(b, l + 1);
    if (!r) r = aexp_6(b, l + 1);
    if (!r) r = listlike(b, l + 1);
    if (!r) r = parenlike(b, l + 1);
    if (!r) r = aexp_9(b, l + 1);
    if (!r) r = gcon(b, l + 1);
    if (!r) r = qvar(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // parensplice exp i ')'
  private static boolean aexp_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENSPLICE);
    r = r && exp(b, l + 1);
    r = r && i(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // singlequote i (qvar | qcon)
  private static boolean aexp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SINGLEQUOTE);
    r = r && i(b, l + 1);
    r = r && aexp_1_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // qvar | qcon
  private static boolean aexp_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_1_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qvar(b, l + 1);
    if (!r) r = qcon(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // thquote i qcon
  private static boolean aexp_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, THQUOTE);
    r = r && i(b, l + 1);
    r = r && qcon(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(#' i '#)'
  private static boolean aexp_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_6")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LUNBOXPAREN);
    r = r && i(b, l + 1);
    r = r && consumeToken(b, RUNBOXPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // [recordlikelhs] i '{' (fbind ',')* [e] (".." | fbind) [e] '}'
  private static boolean aexp_9(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_9")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = aexp_9_0(b, l + 1);
    r = r && i(b, l + 1);
    r = r && consumeToken(b, LBRACE);
    r = r && aexp_9_3(b, l + 1);
    r = r && aexp_9_4(b, l + 1);
    r = r && aexp_9_5(b, l + 1);
    r = r && aexp_9_6(b, l + 1);
    r = r && consumeToken(b, RBRACE);
    exit_section_(b, m, null, r);
    return r;
  }

  // [recordlikelhs]
  private static boolean aexp_9_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_9_0")) return false;
    recordlikelhs(b, l + 1);
    return true;
  }

  // (fbind ',')*
  private static boolean aexp_9_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_9_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!aexp_9_3_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "aexp_9_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // fbind ','
  private static boolean aexp_9_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_9_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fbind(b, l + 1);
    r = r && consumeToken(b, COMMA);
    exit_section_(b, m, null, r);
    return r;
  }

  // [e]
  private static boolean aexp_9_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_9_4")) return false;
    e(b, l + 1);
    return true;
  }

  // ".." | fbind
  private static boolean aexp_9_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_9_5")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLEPERIOD);
    if (!r) r = fbind(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [e]
  private static boolean aexp_9_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "aexp_9_6")) return false;
    e(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '*'
  //         | '!'
  //         | '(' kind ')'
  //         | pkind
  //         | tyvar
  public static boolean akind(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "akind")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<akind>");
    r = consumeToken(b, ASTERISK);
    if (!r) r = consumeToken(b, EXCLAMATION);
    if (!r) r = akind_2(b, l + 1);
    if (!r) r = pkind(b, l + 1);
    if (!r) r = tyvar(b, l + 1);
    exit_section_(b, l, m, AKIND, r, false, null);
    return r;
  }

  // '(' kind ')'
  private static boolean akind_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "akind_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && kind(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // pat i ('->' exp | gdpat) [wheredecls]
  public static boolean alt(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "alt")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<alt>");
    r = pat(b, l + 1);
    r = r && i(b, l + 1);
    r = r && alt_2(b, l + 1);
    p = r; // pin = 3
    r = r && alt_3(b, l + 1);
    exit_section_(b, l, m, ALT, r, p, null);
    return r || p;
  }

  // '->' exp | gdpat
  private static boolean alt_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "alt_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = alt_2_0(b, l + 1);
    if (!r) r = gdpat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '->' exp
  private static boolean alt_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "alt_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, RIGHTARROW);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [wheredecls]
  private static boolean alt_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "alt_3")) return false;
    wheredecls(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '{' alt (semi alt)* '}'
  //                    | iAlts
  static boolean altslist(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "altslist")) return false;
    if (!nextTokenIs(b, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = altslist_0(b, l + 1);
    if (!r) r = iAlts(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '{' alt (semi alt)* '}'
  private static boolean altslist_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "altslist_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACE);
    r = r && alt(b, l + 1);
    r = r && altslist_0_2(b, l + 1);
    r = r && consumeToken(b, RBRACE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (semi alt)*
  private static boolean altslist_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "altslist_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!altslist_0_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "altslist_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // semi alt
  private static boolean altslist_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "altslist_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = semi(b, l + 1);
    r = r && alt(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // literal
  //                | '_'
  //                | ('!'|'~') apat
  //                | var ('+' integertoken | ['@' apat])
  //                | qcon '{' [(fpat ',')* fpat] '}'
  //                | '(' pat (apat* "->" pat | [',' (pat ',')* pat]) ')'
  //                // Second option is quasiquotes. See TemplateHaskell00002.hs.
  //                | '[' (pat (',' pat)* ']' |  exp '|' [semi] exp [semi]'|]')
  //                | gcon
  static boolean apat(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = literal(b, l + 1);
    if (!r) r = consumeToken(b, UNDERSCORE);
    if (!r) r = apat_2(b, l + 1);
    if (!r) r = apat_3(b, l + 1);
    if (!r) r = apat_4(b, l + 1);
    if (!r) r = apat_5(b, l + 1);
    if (!r) r = apat_6(b, l + 1);
    if (!r) r = gcon(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ('!'|'~') apat
  private static boolean apat_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat_2_0(b, l + 1);
    r = r && apat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '!'|'~'
  private static boolean apat_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, EXCLAMATION);
    if (!r) r = consumeToken(b, TILDE);
    exit_section_(b, m, null, r);
    return r;
  }

  // var ('+' integertoken | ['@' apat])
  private static boolean apat_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = var(b, l + 1);
    r = r && apat_3_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '+' integertoken | ['@' apat]
  private static boolean apat_3_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_3_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat_3_1_0(b, l + 1);
    if (!r) r = apat_3_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '+' integertoken
  private static boolean apat_3_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_3_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PLUS);
    r = r && consumeToken(b, INTEGERTOKEN);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['@' apat]
  private static boolean apat_3_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_3_1_1")) return false;
    apat_3_1_1_0(b, l + 1);
    return true;
  }

  // '@' apat
  private static boolean apat_3_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_3_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, AMPERSAT);
    r = r && apat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // qcon '{' [(fpat ',')* fpat] '}'
  private static boolean apat_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_4")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qcon(b, l + 1);
    r = r && consumeToken(b, LBRACE);
    r = r && apat_4_2(b, l + 1);
    r = r && consumeToken(b, RBRACE);
    exit_section_(b, m, null, r);
    return r;
  }

  // [(fpat ',')* fpat]
  private static boolean apat_4_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_4_2")) return false;
    apat_4_2_0(b, l + 1);
    return true;
  }

  // (fpat ',')* fpat
  private static boolean apat_4_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_4_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat_4_2_0_0(b, l + 1);
    r = r && fpat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (fpat ',')*
  private static boolean apat_4_2_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_4_2_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!apat_4_2_0_0_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "apat_4_2_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // fpat ','
  private static boolean apat_4_2_0_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_4_2_0_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fpat(b, l + 1);
    r = r && consumeToken(b, COMMA);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' pat (apat* "->" pat | [',' (pat ',')* pat]) ')'
  private static boolean apat_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && pat(b, l + 1);
    r = r && apat_5_2(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // apat* "->" pat | [',' (pat ',')* pat]
  private static boolean apat_5_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat_5_2_0(b, l + 1);
    if (!r) r = apat_5_2_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // apat* "->" pat
  private static boolean apat_5_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat_5_2_0_0(b, l + 1);
    r = r && consumeToken(b, RIGHTARROW);
    r = r && pat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // apat*
  private static boolean apat_5_2_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5_2_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!apat(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "apat_5_2_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // [',' (pat ',')* pat]
  private static boolean apat_5_2_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5_2_1")) return false;
    apat_5_2_1_0(b, l + 1);
    return true;
  }

  // ',' (pat ',')* pat
  private static boolean apat_5_2_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5_2_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && apat_5_2_1_0_1(b, l + 1);
    r = r && pat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (pat ',')*
  private static boolean apat_5_2_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5_2_1_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!apat_5_2_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "apat_5_2_1_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // pat ','
  private static boolean apat_5_2_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_5_2_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pat(b, l + 1);
    r = r && consumeToken(b, COMMA);
    exit_section_(b, m, null, r);
    return r;
  }

  // '[' (pat (',' pat)* ']' |  exp '|' [semi] exp [semi]'|]')
  private static boolean apat_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACKET);
    r = r && apat_6_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // pat (',' pat)* ']' |  exp '|' [semi] exp [semi]'|]'
  private static boolean apat_6_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat_6_1_0(b, l + 1);
    if (!r) r = apat_6_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // pat (',' pat)* ']'
  private static boolean apat_6_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pat(b, l + 1);
    r = r && apat_6_1_0_1(b, l + 1);
    r = r && consumeToken(b, RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' pat)*
  private static boolean apat_6_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6_1_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!apat_6_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "apat_6_1_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' pat
  private static boolean apat_6_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && pat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // exp '|' [semi] exp [semi]'|]'
  private static boolean apat_6_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && consumeToken(b, PIPE);
    r = r && apat_6_1_1_2(b, l + 1);
    r = r && exp(b, l + 1);
    r = r && apat_6_1_1_4(b, l + 1);
    r = r && consumeToken(b, RTHCLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // [semi]
  private static boolean apat_6_1_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6_1_1_2")) return false;
    semi(b, l + 1);
    return true;
  }

  // [semi]
  private static boolean apat_6_1_1_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "apat_6_1_1_4")) return false;
    semi(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // "type" ["instance"] (typee [kindsig] | ctype '=' ctype)
  //                  | "data" ["instance"] ctype [kindsig]
  static boolean atdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl")) return false;
    if (!nextTokenIs(b, "", DATA, TYPE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atdecl_0(b, l + 1);
    if (!r) r = atdecl_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "type" ["instance"] (typee [kindsig] | ctype '=' ctype)
  private static boolean atdecl_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, TYPE);
    r = r && atdecl_0_1(b, l + 1);
    r = r && atdecl_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["instance"]
  private static boolean atdecl_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_0_1")) return false;
    consumeToken(b, INSTANCE);
    return true;
  }

  // typee [kindsig] | ctype '=' ctype
  private static boolean atdecl_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_0_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atdecl_0_2_0(b, l + 1);
    if (!r) r = atdecl_0_2_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // typee [kindsig]
  private static boolean atdecl_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typee(b, l + 1);
    r = r && atdecl_0_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [kindsig]
  private static boolean atdecl_0_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_0_2_0_1")) return false;
    kindsig(b, l + 1);
    return true;
  }

  // ctype '=' ctype
  private static boolean atdecl_0_2_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_0_2_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = ctype(b, l + 1);
    r = r && consumeToken(b, EQUALS);
    r = r && ctype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "data" ["instance"] ctype [kindsig]
  private static boolean atdecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DATA);
    r = r && atdecl_1_1(b, l + 1);
    r = r && ctype(b, l + 1);
    r = r && atdecl_1_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["instance"]
  private static boolean atdecl_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_1_1")) return false;
    consumeToken(b, INSTANCE);
    return true;
  }

  // [kindsig]
  private static boolean atdecl_1_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atdecl_1_3")) return false;
    kindsig(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // [singlequote] ntgtycon ['##'|'#']
  //         | tyvar
  //         | strict_mark atype
  //         | '{' fielddecls '}'
  //         | '(#' <<sequence ctype>> '#)'
  //         | '(' ['?'] ctype "::" (kind | ctype)')'
  //         | [singlequote] ('(' [<<sequence ctype>>] ')' | '[' <<sequence ctype>> ']')
  //         | integertoken
  //         | pstringtoken
  //         | foralltype
  public static boolean atype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<atype>");
    r = atype_0(b, l + 1);
    if (!r) r = tyvar(b, l + 1);
    if (!r) r = atype_2(b, l + 1);
    if (!r) r = atype_3(b, l + 1);
    if (!r) r = atype_4(b, l + 1);
    if (!r) r = atype_5(b, l + 1);
    if (!r) r = atype_6(b, l + 1);
    if (!r) r = consumeToken(b, INTEGERTOKEN);
    if (!r) r = pstringtoken(b, l + 1);
    if (!r) r = foralltype(b, l + 1);
    exit_section_(b, l, m, ATYPE, r, false, null);
    return r;
  }

  // [singlequote] ntgtycon ['##'|'#']
  private static boolean atype_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atype_0_0(b, l + 1);
    r = r && ntgtycon(b, l + 1);
    r = r && atype_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [singlequote]
  private static boolean atype_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_0_0")) return false;
    consumeToken(b, SINGLEQUOTE);
    return true;
  }

  // ['##'|'#']
  private static boolean atype_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_0_2")) return false;
    atype_0_2_0(b, l + 1);
    return true;
  }

  // '##'|'#'
  private static boolean atype_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLEHASH);
    if (!r) r = consumeToken(b, HASH);
    exit_section_(b, m, null, r);
    return r;
  }

  // strict_mark atype
  private static boolean atype_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = strict_mark(b, l + 1);
    r = r && atype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '{' fielddecls '}'
  private static boolean atype_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACE);
    r = r && fielddecls(b, l + 1);
    r = r && consumeToken(b, RBRACE);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(#' <<sequence ctype>> '#)'
  private static boolean atype_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_4")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LUNBOXPAREN);
    r = r && sequence(b, l + 1, ctype_parser_);
    r = r && consumeToken(b, RUNBOXPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' ['?'] ctype "::" (kind | ctype)')'
  private static boolean atype_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_5")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && atype_5_1(b, l + 1);
    r = r && ctype(b, l + 1);
    r = r && consumeToken(b, DOUBLECOLON);
    r = r && atype_5_4(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['?']
  private static boolean atype_5_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_5_1")) return false;
    consumeToken(b, QUESTION);
    return true;
  }

  // kind | ctype
  private static boolean atype_5_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_5_4")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = kind(b, l + 1);
    if (!r) r = ctype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [singlequote] ('(' [<<sequence ctype>>] ')' | '[' <<sequence ctype>> ']')
  private static boolean atype_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_6")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atype_6_0(b, l + 1);
    r = r && atype_6_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [singlequote]
  private static boolean atype_6_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_6_0")) return false;
    consumeToken(b, SINGLEQUOTE);
    return true;
  }

  // '(' [<<sequence ctype>>] ')' | '[' <<sequence ctype>> ']'
  private static boolean atype_6_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_6_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atype_6_1_0(b, l + 1);
    if (!r) r = atype_6_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' [<<sequence ctype>>] ')'
  private static boolean atype_6_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_6_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && atype_6_1_0_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // [<<sequence ctype>>]
  private static boolean atype_6_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_6_1_0_1")) return false;
    sequence(b, l + 1, ctype_parser_);
    return true;
  }

  // '[' <<sequence ctype>> ']'
  private static boolean atype_6_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atype_6_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACKET);
    r = r && sequence(b, l + 1, ctype_parser_);
    r = r && consumeToken(b, RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // akind [bkind]
  public static boolean bkind(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bkind")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<bkind>");
    r = akind(b, l + 1);
    r = r && bkind_1(b, l + 1);
    exit_section_(b, l, m, BKIND, r, false, null);
    return r;
  }

  // [bkind]
  private static boolean bkind_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bkind_1")) return false;
    bkind(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // ppragma* open bodyaux close
  public static boolean body(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "body")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<body>");
    r = body_0(b, l + 1);
    r = r && open(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, bodyaux(b, l + 1));
    r = p && close(b, l + 1) && r;
    exit_section_(b, l, m, BODY, r, p, null);
    return r || p;
  }

  // ppragma*
  private static boolean body_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "body_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "body_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // impdecls* ppragma* [[semi] topdecls]
  static boolean bodyaux(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bodyaux")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = bodyaux_0(b, l + 1);
    r = r && bodyaux_1(b, l + 1);
    r = r && bodyaux_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // impdecls*
  private static boolean bodyaux_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bodyaux_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!impdecls(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "bodyaux_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ppragma*
  private static boolean bodyaux_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bodyaux_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "bodyaux_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // [[semi] topdecls]
  private static boolean bodyaux_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bodyaux_2")) return false;
    bodyaux_2_0(b, l + 1);
    return true;
  }

  // [semi] topdecls
  private static boolean bodyaux_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bodyaux_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = bodyaux_2_0_0(b, l + 1);
    r = r && topdecls(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [semi]
  private static boolean bodyaux_2_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bodyaux_2_0_0")) return false;
    semi(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // atype [btype]
  static boolean btype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "btype")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atype(b, l + 1);
    r = r && btype_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [btype]
  private static boolean btype_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "btype_1")) return false;
    btype(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // "ccall" | "stdcall" | "cplusplus"
  //                    | "jvm" | "dotnet"
  static boolean callconv(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "callconv")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "ccall");
    if (!r) r = consumeToken(b, "stdcall");
    if (!r) r = consumeToken(b, "cplusplus");
    if (!r) r = consumeToken(b, "jvm");
    if (!r) r = consumeToken(b, "dotnet");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // atdecl
  //         | (funlhs | var) rhs
  //         | gendecl
  public static boolean cdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecl")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<cdecl>");
    r = atdecl(b, l + 1);
    if (!r) r = cdecl_1(b, l + 1);
    if (!r) r = gendecl(b, l + 1);
    exit_section_(b, l, m, CDECL, r, false, null);
    return r;
  }

  // (funlhs | var) rhs
  private static boolean cdecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecl_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = cdecl_1_0(b, l + 1);
    r = r && rhs(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // funlhs | var
  private static boolean cdecl_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecl_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = funlhs(b, l + 1);
    if (!r) r = var(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // open [cdecls1] close
  static boolean cdecls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecls")) return false;
    if (!nextTokenIs(b, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = open(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, cdecls_1(b, l + 1));
    r = p && close(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [cdecls1]
  private static boolean cdecls_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecls_1")) return false;
    cdecls1(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // cdecl [semi cdecls1]
  static boolean cdecls1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecls1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = cdecl(b, l + 1);
    r = r && cdecls1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [semi cdecls1]
  private static boolean cdecls1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecls1_1")) return false;
    cdecls1_1_0(b, l + 1);
    return true;
  }

  // semi cdecls1
  private static boolean cdecls1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cdecls1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = semi(b, l + 1);
    r = r && cdecls1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "class" ctype [fundeps] ["where" cdecls]
  public static boolean classdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "classdecl")) return false;
    if (!nextTokenIs(b, CLASSTOKEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, CLASSTOKEN);
    p = r; // pin = 1
    r = r && report_error_(b, ctype(b, l + 1));
    r = p && report_error_(b, classdecl_2(b, l + 1)) && r;
    r = p && classdecl_3(b, l + 1) && r;
    exit_section_(b, l, m, CLASSDECL, r, p, null);
    return r || p;
  }

  // [fundeps]
  private static boolean classdecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "classdecl_2")) return false;
    fundeps(b, l + 1);
    return true;
  }

  // ["where" cdecls]
  private static boolean classdecl_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "classdecl_3")) return false;
    classdecl_3_0(b, l + 1);
    return true;
  }

  // "where" cdecls
  private static boolean classdecl_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "classdecl_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, WHERE);
    r = r && cdecls(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '}'
  //                 | WHITESPACERBRACETOK
  //                 | [<<stateHackMess>>]
  static boolean close(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "close")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, RBRACE);
    if (!r) r = consumeToken(b, WHITESPACERBRACETOK);
    if (!r) r = close_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [<<stateHackMess>>]
  private static boolean close_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "close_2")) return false;
    stateHackMess(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // var | con
  static boolean cname(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cname")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = var(b, l + 1);
    if (!r) r = con(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<sequence cname>>
  static boolean cnames(PsiBuilder b, int l) {
    return sequence(b, l + 1, cname_parser_);
  }

  /* ********************************************************** */
  // '(' <<p>> (',' <<p>>)* ')'
  static boolean commaSeparate(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate")) return false;
    if (!nextTokenIs(b, LPAREN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, LPAREN);
    p = r; // pin = 1
    r = r && report_error_(b, _p.parse(b, l));
    r = p && report_error_(b, commaSeparate_2(b, l + 1, _p)) && r;
    r = p && consumeToken(b, RPAREN) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' <<p>>)*
  private static boolean commaSeparate_2(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!commaSeparate_2_0(b, l + 1, _p)) break;
      if (!empty_element_parsed_guard_(b, "commaSeparate_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' <<p>>
  private static boolean commaSeparate_2_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate_2_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, COMMA);
    p = r; // pin = 1
    r = r && _p.parse(b, l);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '(' <<p>> (',' (<<p>> | &')'))* ')'
  static boolean commaSeparate2(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate2")) return false;
    if (!nextTokenIs(b, LPAREN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, LPAREN);
    p = r; // pin = 1
    r = r && report_error_(b, _p.parse(b, l));
    r = p && report_error_(b, commaSeparate2_2(b, l + 1, _p)) && r;
    r = p && consumeToken(b, RPAREN) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' (<<p>> | &')'))*
  private static boolean commaSeparate2_2(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate2_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!commaSeparate2_2_0(b, l + 1, _p)) break;
      if (!empty_element_parsed_guard_(b, "commaSeparate2_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' (<<p>> | &')')
  private static boolean commaSeparate2_2_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate2_2_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, COMMA);
    p = r; // pin = 1
    r = r && commaSeparate2_2_0_1(b, l + 1, _p);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // <<p>> | &')'
  private static boolean commaSeparate2_2_0_1(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate2_2_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = _p.parse(b, l);
    if (!r) r = commaSeparate2_2_0_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &')'
  private static boolean commaSeparate2_2_0_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "commaSeparate2_2_0_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = consumeToken(b, RPAREN);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // kind  [',' comma_kinds1]
  static boolean comma_kinds1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comma_kinds1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = kind(b, l + 1);
    r = r && comma_kinds1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [',' comma_kinds1]
  private static boolean comma_kinds1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comma_kinds1_1")) return false;
    comma_kinds1_1_0(b, l + 1);
    return true;
  }

  // ',' comma_kinds1
  private static boolean comma_kinds1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comma_kinds1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && comma_kinds1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ','+
  static boolean commas(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "commas")) return false;
    if (!nextTokenIs(b, COMMA)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    int c = current_position_(b);
    while (r) {
      if (!consumeToken(b, COMMA)) break;
      if (!empty_element_parsed_guard_(b, "commas", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // conid | '(' consym ')'
  public static boolean con(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "con")) return false;
    if (!nextTokenIs(b, "<con>", LPAREN, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<con>");
    r = conid(b, l + 1);
    if (!r) r = con_1(b, l + 1);
    exit_section_(b, l, m, CON, r, false, null);
    return r;
  }

  // '(' consym ')'
  private static boolean con_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "con_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && consym(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // conidRegexp
  public static boolean conid(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conid")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, CONIDREGEXP);
    exit_section_(b, m, CONID, r);
    return r;
  }

  /* ********************************************************** */
  // consym | '`' conid '`'
  public static boolean conop(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conop")) return false;
    if (!nextTokenIs(b, "<conop>", BACKTICK, CONSYMTOK)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<conop>");
    r = consym(b, l + 1);
    if (!r) r = conop_1(b, l + 1);
    exit_section_(b, l, m, CONOP, r, false, null);
    return r;
  }

  // '`' conid '`'
  private static boolean conop_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conop_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BACKTICK);
    r = r && conid(b, l + 1);
    r = r && consumeToken(b, BACKTICK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // btype conop btype
  //          | con ('{' [(fielddecl ',')* fielddecl] '}' | btype*)
  public static boolean constr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<constr>");
    r = constr_0(b, l + 1);
    if (!r) r = constr_1(b, l + 1);
    exit_section_(b, l, m, CONSTR, r, false, null);
    return r;
  }

  // btype conop btype
  private static boolean constr_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = btype(b, l + 1);
    r = r && conop(b, l + 1);
    r = r && btype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // con ('{' [(fielddecl ',')* fielddecl] '}' | btype*)
  private static boolean constr_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = con(b, l + 1);
    r = r && constr_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '{' [(fielddecl ',')* fielddecl] '}' | btype*
  private static boolean constr_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = constr_1_1_0(b, l + 1);
    if (!r) r = constr_1_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '{' [(fielddecl ',')* fielddecl] '}'
  private static boolean constr_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACE);
    r = r && constr_1_1_0_1(b, l + 1);
    r = r && consumeToken(b, RBRACE);
    exit_section_(b, m, null, r);
    return r;
  }

  // [(fielddecl ',')* fielddecl]
  private static boolean constr_1_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1_1_0_1")) return false;
    constr_1_1_0_1_0(b, l + 1);
    return true;
  }

  // (fielddecl ',')* fielddecl
  private static boolean constr_1_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = constr_1_1_0_1_0_0(b, l + 1);
    r = r && fielddecl(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (fielddecl ',')*
  private static boolean constr_1_1_0_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1_1_0_1_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!constr_1_1_0_1_0_0_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "constr_1_1_0_1_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // fielddecl ','
  private static boolean constr_1_1_0_1_0_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1_1_0_1_0_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fielddecl(b, l + 1);
    r = r && consumeToken(b, COMMA);
    exit_section_(b, m, null, r);
    return r;
  }

  // btype*
  private static boolean constr_1_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_1_1_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!btype(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "constr_1_1_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // [context "=>"] constr ('|' constr)*
  static boolean constrs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constrs")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = constrs_0(b, l + 1);
    r = r && constr(b, l + 1);
    r = r && constrs_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [context "=>"]
  private static boolean constrs_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constrs_0")) return false;
    constrs_0_0(b, l + 1);
    return true;
  }

  // context "=>"
  private static boolean constrs_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constrs_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = context(b, l + 1);
    r = r && consumeToken(b, DOUBLEARROW);
    exit_section_(b, m, null, r);
    return r;
  }

  // ('|' constr)*
  private static boolean constrs_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constrs_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!constrs_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "constrs_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '|' constr
  private static boolean constrs_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constrs_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PIPE);
    r = r && constr(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // CONSYMTOK
  public static boolean consym(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "consym")) return false;
    if (!nextTokenIs(b, CONSYMTOK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, CONSYMTOK);
    exit_section_(b, m, CONSYM, r);
    return r;
  }

  /* ********************************************************** */
  // btype ['~' btype]
  public static boolean context(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "context")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<context>");
    r = btype(b, l + 1);
    r = r && context_1(b, l + 1);
    exit_section_(b, l, m, CONTEXT, r, false, null);
    return r;
  }

  // ['~' btype]
  private static boolean context_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "context_1")) return false;
    context_1_0(b, l + 1);
    return true;
  }

  // '~' btype
  private static boolean context_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "context_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, TILDE);
    r = r && btype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // context '=>' ctype
  static boolean contexttype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "contexttype")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = context(b, l + 1);
    r = r && consumeToken(b, DOUBLEARROW);
    p = r; // pin = 2
    r = r && ctype(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // foralltype
  //         | contexttype
  //         | typee
  public static boolean ctype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ctype")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<ctype>");
    r = foralltype(b, l + 1);
    if (!r) r = contexttype(b, l + 1);
    if (!r) r = typee(b, l + 1);
    exit_section_(b, l, m, CTYPE, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // "data" ["instance"] [context "=>"] typee ['=' ["forall" tv_bndr* '.'] constrs| [kindsig] ["where" gadtconstrs]] [deriving]
  public static boolean datadecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl")) return false;
    if (!nextTokenIs(b, DATA)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, DATA);
    p = r; // pin = 1
    r = r && report_error_(b, datadecl_1(b, l + 1));
    r = p && report_error_(b, datadecl_2(b, l + 1)) && r;
    r = p && report_error_(b, typee(b, l + 1)) && r;
    r = p && report_error_(b, datadecl_4(b, l + 1)) && r;
    r = p && datadecl_5(b, l + 1) && r;
    exit_section_(b, l, m, DATADECL, r, p, null);
    return r || p;
  }

  // ["instance"]
  private static boolean datadecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_1")) return false;
    consumeToken(b, INSTANCE);
    return true;
  }

  // [context "=>"]
  private static boolean datadecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_2")) return false;
    datadecl_2_0(b, l + 1);
    return true;
  }

  // context "=>"
  private static boolean datadecl_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = context(b, l + 1);
    r = r && consumeToken(b, DOUBLEARROW);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['=' ["forall" tv_bndr* '.'] constrs| [kindsig] ["where" gadtconstrs]]
  private static boolean datadecl_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4")) return false;
    datadecl_4_0(b, l + 1);
    return true;
  }

  // '=' ["forall" tv_bndr* '.'] constrs| [kindsig] ["where" gadtconstrs]
  private static boolean datadecl_4_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = datadecl_4_0_0(b, l + 1);
    if (!r) r = datadecl_4_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '=' ["forall" tv_bndr* '.'] constrs
  private static boolean datadecl_4_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, EQUALS);
    r = r && datadecl_4_0_0_1(b, l + 1);
    r = r && constrs(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["forall" tv_bndr* '.']
  private static boolean datadecl_4_0_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_0_1")) return false;
    datadecl_4_0_0_1_0(b, l + 1);
    return true;
  }

  // "forall" tv_bndr* '.'
  private static boolean datadecl_4_0_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, FORALLTOKEN);
    r = r && datadecl_4_0_0_1_0_1(b, l + 1);
    r = r && consumeToken(b, PERIOD);
    exit_section_(b, m, null, r);
    return r;
  }

  // tv_bndr*
  private static boolean datadecl_4_0_0_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_0_1_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!tv_bndr(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "datadecl_4_0_0_1_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // [kindsig] ["where" gadtconstrs]
  private static boolean datadecl_4_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = datadecl_4_0_1_0(b, l + 1);
    r = r && datadecl_4_0_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [kindsig]
  private static boolean datadecl_4_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_1_0")) return false;
    kindsig(b, l + 1);
    return true;
  }

  // ["where" gadtconstrs]
  private static boolean datadecl_4_0_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_1_1")) return false;
    datadecl_4_0_1_1_0(b, l + 1);
    return true;
  }

  // "where" gadtconstrs
  private static boolean datadecl_4_0_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_4_0_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, WHERE);
    r = r && gadtconstrs(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [deriving]
  private static boolean datadecl_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datadecl_5")) return false;
    deriving(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // qtycls (dclass|tyvar)*
  static boolean dclass(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dclass")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qtycls(b, l + 1);
    r = r && dclass_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (dclass|tyvar)*
  private static boolean dclass_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dclass_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!dclass_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "dclass_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // dclass|tyvar
  private static boolean dclass_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dclass_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = dclass(b, l + 1);
    if (!r) r = tyvar(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // funorpatdecl
  //                | gendecl
  static boolean decl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decl")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = funorpatdecl(b, l + 1);
    if (!r) r = gendecl(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // open [decls1] close
  static boolean decls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decls")) return false;
    if (!nextTokenIs(b, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = open(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, decls_1(b, l + 1));
    r = p && close(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [decls1]
  private static boolean decls_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decls_1")) return false;
    decls1(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // ppragma* decl ppragma* [semi decls1]
  static boolean decls1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decls1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = decls1_0(b, l + 1);
    r = r && decl(b, l + 1);
    r = r && decls1_2(b, l + 1);
    r = r && decls1_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ppragma*
  private static boolean decls1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decls1_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "decls1_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ppragma*
  private static boolean decls1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decls1_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "decls1_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // [semi decls1]
  private static boolean decls1_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decls1_3")) return false;
    decls1_3_0(b, l + 1);
    return true;
  }

  // semi decls1
  private static boolean decls1_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "decls1_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = semi(b, l + 1);
    r = r && decls1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "default" <<commaSeparate typee>>
  public static boolean defaultdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "defaultdecl")) return false;
    if (!nextTokenIs(b, DEFAULT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, DEFAULT);
    p = r; // pin = 1
    r = r && commaSeparate(b, l + 1, typee_parser_);
    exit_section_(b, l, m, DEFAULTDECL, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // "deriving" (dclass  | <<commaSeparate dclass>>)
  static boolean deriving(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "deriving")) return false;
    if (!nextTokenIs(b, DERIVING)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, DERIVING);
    p = r; // pin = 1
    r = r && deriving_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // dclass  | <<commaSeparate dclass>>
  private static boolean deriving_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "deriving_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = dclass(b, l + 1);
    if (!r) r = commaSeparate(b, l + 1, dclass_parser_);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "deriving" "instance" [ppragma] ctype
  public static boolean derivingdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "derivingdecl")) return false;
    if (!nextTokenIs(b, DERIVING)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, DERIVING);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, INSTANCE));
    r = p && report_error_(b, derivingdecl_2(b, l + 1)) && r;
    r = p && ctype(b, l + 1) && r;
    exit_section_(b, l, m, DERIVINGDECL, r, p, null);
    return r || p;
  }

  // [ppragma]
  private static boolean derivingdecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "derivingdecl_2")) return false;
    ppragma(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // <<indented true>>
  static boolean e(PsiBuilder b, int l) {
    return indented(b, l + 1, true);
  }

  /* ********************************************************** */
  // ppragma* infixexp ["::" [context "=>"] typee]
  public static boolean exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<exp>");
    r = exp_0(b, l + 1);
    r = r && infixexp(b, l + 1);
    r = r && exp_2(b, l + 1);
    exit_section_(b, l, m, EXP, r, false, null);
    return r;
  }

  // ppragma*
  private static boolean exp_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "exp_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ["::" [context "=>"] typee]
  private static boolean exp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_2")) return false;
    exp_2_0(b, l + 1);
    return true;
  }

  // "::" [context "=>"] typee
  private static boolean exp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLECOLON);
    r = r && exp_2_0_1(b, l + 1);
    r = r && typee(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [context "=>"]
  private static boolean exp_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_2_0_1")) return false;
    exp_2_0_1_0(b, l + 1);
    return true;
  }

  // context "=>"
  private static boolean exp_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = context(b, l + 1);
    r = r && consumeToken(b, DOUBLEARROW);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // pstringtoken
  static boolean expent(PsiBuilder b, int l) {
    return pstringtoken(b, l + 1);
  }

  /* ********************************************************** */
  // "module" qconid
  //         // Really (qtycon|qtycls) but they are both ::= qconid.
  //         | qtycon [ '(' (".." | cnames | qvars) ')']
  //         | qvar
  public static boolean export(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<export>");
    r = export_0(b, l + 1);
    if (!r) r = export_1(b, l + 1);
    if (!r) r = qvar(b, l + 1);
    exit_section_(b, l, m, EXPORT, r, false, null);
    return r;
  }

  // "module" qconid
  private static boolean export_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, MODULETOKEN);
    p = r; // pin = 1
    r = r && qconid(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // qtycon [ '(' (".." | cnames | qvars) ')']
  private static boolean export_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = qtycon(b, l + 1);
    p = r; // pin = 1
    r = r && export_1_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [ '(' (".." | cnames | qvars) ')']
  private static boolean export_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_1_1")) return false;
    export_1_1_0(b, l + 1);
    return true;
  }

  // '(' (".." | cnames | qvars) ')'
  private static boolean export_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_1_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, LPAREN);
    p = r; // pin = 1
    r = r && report_error_(b, export_1_1_0_1(b, l + 1));
    r = p && consumeToken(b, RPAREN) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ".." | cnames | qvars
  private static boolean export_1_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_1_1_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLEPERIOD);
    if (!r) r = cnames(b, l + 1);
    if (!r) r = qvars(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "export" callconv [expent]
  static boolean exportdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exportdecl")) return false;
    if (!nextTokenIs(b, EXPORTTOKEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, EXPORTTOKEN);
    p = r; // pin = 1
    r = r && report_error_(b, callconv(b, l + 1));
    r = p && exportdecl_2(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [expent]
  private static boolean exportdecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exportdecl_2")) return false;
    expent(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // <<commaSeparate2 export>>
  public static boolean exports(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exports")) return false;
    if (!nextTokenIs(b, LPAREN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = commaSeparate2(b, l + 1, export_parser_);
    exit_section_(b, m, EXPORTS, r);
    return r;
  }

  /* ********************************************************** */
  // "()" | '(' ')'
  public static boolean exportsempty(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exportsempty")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<exportsempty>");
    r = consumeToken(b, "()");
    if (!r) r = exportsempty_1(b, l + 1);
    exit_section_(b, l, m, EXPORTSEMPTY, r, false, null);
    return r;
  }

  // '(' ')'
  private static boolean exportsempty_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exportsempty_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // qtycon atype*
  static boolean fatype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fatype")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qtycon(b, l + 1);
    r = r && fatype_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // atype*
  private static boolean fatype_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fatype_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!atype(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "fatype_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // qvar '=' exp
  static boolean fbind(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fbind")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = qvar(b, l + 1);
    r = r && consumeToken(b, EQUALS);
    p = r; // pin = 2
    r = r && exp(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // (importdecl | exportdecl) var "::" ftype
  static boolean fdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fdecl")) return false;
    if (!nextTokenIs(b, "", EXPORTTOKEN, IMPORT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fdecl_0(b, l + 1);
    r = r && var(b, l + 1);
    r = r && consumeToken(b, DOUBLECOLON);
    r = r && ftype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // importdecl | exportdecl
  private static boolean fdecl_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fdecl_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = importdecl(b, l + 1);
    if (!r) r = exportdecl(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // vars "::" (typee | '!' atype)
  static boolean fielddecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fielddecl")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = vars(b, l + 1);
    r = r && consumeToken(b, DOUBLECOLON);
    p = r; // pin = 2
    r = r && fielddecl_2(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // typee | '!' atype
  private static boolean fielddecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fielddecl_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typee(b, l + 1);
    if (!r) r = fielddecl_2_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '!' atype
  private static boolean fielddecl_2_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fielddecl_2_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, EXCLAMATION);
    r = r && atype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // fielddecl [',' fielddecls]
  static boolean fielddecls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fielddecls")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = fielddecl(b, l + 1);
    p = r; // pin = 1
    r = r && fielddecls_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [',' fielddecls]
  private static boolean fielddecls_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fielddecls_1")) return false;
    fielddecls_1_0(b, l + 1);
    return true;
  }

  // ',' fielddecls
  private static boolean fielddecls_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fielddecls_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, COMMA);
    p = r; // pin = 1
    r = r && fielddecls(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // "infix" | "infixr" | "infixl"
  public static boolean fixity(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fixity")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<fixity>");
    r = consumeToken(b, INFIX);
    if (!r) r = consumeToken(b, INFIXR);
    if (!r) r = consumeToken(b, INFIXL);
    exit_section_(b, l, m, FIXITY, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // "forall" tv_bndr* '.' ctype
  static boolean foralltype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "foralltype")) return false;
    if (!nextTokenIs(b, FORALLTOKEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, FORALLTOKEN);
    p = r; // pin = 1
    r = r && report_error_(b, foralltype_1(b, l + 1));
    r = p && report_error_(b, consumeToken(b, PERIOD)) && r;
    r = p && ctype(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // tv_bndr*
  private static boolean foralltype_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "foralltype_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!tv_bndr(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "foralltype_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // "foreign" fdecl
  public static boolean foreigndecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "foreigndecl")) return false;
    if (!nextTokenIs(b, FOREIGN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, FOREIGN);
    p = r; // pin = 1
    r = r && fdecl(b, l + 1);
    exit_section_(b, l, m, FOREIGNDECL, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // qvar ['=' pat]
  //                | ".."
  static boolean fpat(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fpat")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fpat_0(b, l + 1);
    if (!r) r = consumeToken(b, DOUBLEPERIOD);
    exit_section_(b, m, null, r);
    return r;
  }

  // qvar ['=' pat]
  private static boolean fpat_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fpat_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = qvar(b, l + 1);
    p = r; // pin = 1
    r = r && fpat_0_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ['=' pat]
  private static boolean fpat_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fpat_0_1")) return false;
    fpat_0_1_0(b, l + 1);
    return true;
  }

  // '=' pat
  private static boolean fpat_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fpat_0_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, EQUALS);
    p = r; // pin = 1
    r = r && pat(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // fatype ["->" ftype]
  //                 | "()"
  static boolean ftype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ftype")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = ftype_0(b, l + 1);
    if (!r) r = consumeToken(b, "()");
    exit_section_(b, m, null, r);
    return r;
  }

  // fatype ["->" ftype]
  private static boolean ftype_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ftype_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fatype(b, l + 1);
    r = r && ftype_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["->" ftype]
  private static boolean ftype_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ftype_0_1")) return false;
    ftype_0_1_0(b, l + 1);
    return true;
  }

  // "->" ftype
  private static boolean ftype_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ftype_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, RIGHTARROW);
    r = r && ftype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // tyvar+ "->" tyvar+
  static boolean fundep(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fundep")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = fundep_0(b, l + 1);
    r = r && consumeToken(b, RIGHTARROW);
    p = r; // pin = 2
    r = r && fundep_2(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // tyvar+
  private static boolean fundep_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fundep_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tyvar(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!tyvar(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "fundep_0", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // tyvar+
  private static boolean fundep_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fundep_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tyvar(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!tyvar(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "fundep_2", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '|' <<sequence fundep>>
  static boolean fundeps(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fundeps")) return false;
    if (!nextTokenIs(b, PIPE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, PIPE);
    p = r; // pin = 1
    r = r && sequence(b, l + 1, fundep_parser_);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // (var |'(' funlhs ')') apat+
  //                  | pat varop pat
  static boolean funlhs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funlhs")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = funlhs_0(b, l + 1);
    if (!r) r = funlhs_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (var |'(' funlhs ')') apat+
  private static boolean funlhs_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funlhs_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = funlhs_0_0(b, l + 1);
    r = r && funlhs_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // var |'(' funlhs ')'
  private static boolean funlhs_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funlhs_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = var(b, l + 1);
    if (!r) r = funlhs_0_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' funlhs ')'
  private static boolean funlhs_0_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funlhs_0_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && funlhs(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // apat+
  private static boolean funlhs_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funlhs_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!apat(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "funlhs_0_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // pat varop pat
  private static boolean funlhs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funlhs_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pat(b, l + 1);
    r = r && varop(b, l + 1);
    r = r && pat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // (funlhs | pat) rhs
  public static boolean funorpatdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funorpatdecl")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<funorpatdecl>");
    r = funorpatdecl_0(b, l + 1);
    r = r && rhs(b, l + 1);
    exit_section_(b, l, m, FUNORPATDECL, r, false, null);
    return r;
  }

  // funlhs | pat
  private static boolean funorpatdecl_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "funorpatdecl_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = funlhs(b, l + 1);
    if (!r) r = pat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<sequence con>> "::" ctype
  //                      | oqtycon '{' fielddecls '}' "::" ctype
  static boolean gadtconstr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gadtconstr")) return false;
    if (!nextTokenIs(b, "", LPAREN, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = gadtconstr_0(b, l + 1);
    if (!r) r = gadtconstr_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // <<sequence con>> "::" ctype
  private static boolean gadtconstr_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gadtconstr_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = sequence(b, l + 1, con_parser_);
    r = r && consumeToken(b, DOUBLECOLON);
    r = r && ctype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // oqtycon '{' fielddecls '}' "::" ctype
  private static boolean gadtconstr_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gadtconstr_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = oqtycon(b, l + 1);
    r = r && consumeToken(b, LBRACE);
    r = r && fielddecls(b, l + 1);
    r = r && consumeToken(b, RBRACE);
    r = r && consumeToken(b, DOUBLECOLON);
    r = r && ctype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // open [gadtconstrs1] close
  static boolean gadtconstrs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gadtconstrs")) return false;
    if (!nextTokenIs(b, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && gadtconstrs_1(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [gadtconstrs1]
  private static boolean gadtconstrs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gadtconstrs_1")) return false;
    gadtconstrs1(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // gadtconstr semi gadtconstrs1
  //                        | gadtconstr
  static boolean gadtconstrs1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gadtconstrs1")) return false;
    if (!nextTokenIs(b, "", LPAREN, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = gadtconstrs1_0(b, l + 1);
    if (!r) r = gadtconstr(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // gadtconstr semi gadtconstrs1
  private static boolean gadtconstrs1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gadtconstrs1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = gadtconstr(b, l + 1);
    r = r && semi(b, l + 1);
    r = r && gadtconstrs1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '[' ']'
  //               | '(' [commas] ')'
  //               | qcon
  static boolean gcon(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gcon")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = gcon_0(b, l + 1);
    if (!r) r = gcon_1(b, l + 1);
    if (!r) r = qcon(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '[' ']'
  private static boolean gcon_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gcon_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACKET);
    r = r && consumeToken(b, RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' [commas] ')'
  private static boolean gcon_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gcon_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && gcon_1_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // [commas]
  private static boolean gcon_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gcon_1_1")) return false;
    commas(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // ':' | qconsym
  public static boolean gconsym(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gconsym")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<gconsym>");
    r = consumeToken(b, COLON);
    if (!r) r = qconsym(b, l + 1);
    exit_section_(b, l, m, GCONSYM, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // guards '->' exp [gdpat]
  static boolean gdpat(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gdpat")) return false;
    if (!nextTokenIs(b, PIPE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = guards(b, l + 1);
    r = r && consumeToken(b, RIGHTARROW);
    p = r; // pin = 2
    r = r && report_error_(b, exp(b, l + 1));
    r = p && gdpat_3(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [gdpat]
  private static boolean gdpat_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gdpat_3")) return false;
    gdpat(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // guards '=' exp
  static boolean gdrhs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gdrhs")) return false;
    if (!nextTokenIs(b, PIPE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = guards(b, l + 1);
    r = r && consumeToken(b, EQUALS);
    p = r; // pin = 2
    r = r && exp(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // gendeclfst
  //           | fixity [integertoken] (<<sequence op>>)
  public static boolean gendecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gendecl")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<gendecl>");
    r = gendeclfst(b, l + 1);
    if (!r) r = gendecl_1(b, l + 1);
    exit_section_(b, l, m, GENDECL, r, false, null);
    return r;
  }

  // fixity [integertoken] (<<sequence op>>)
  private static boolean gendecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gendecl_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = fixity(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, gendecl_1_1(b, l + 1));
    r = p && gendecl_1_2(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [integertoken]
  private static boolean gendecl_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gendecl_1_1")) return false;
    consumeToken(b, INTEGERTOKEN);
    return true;
  }

  // <<sequence op>>
  private static boolean gendecl_1_2(PsiBuilder b, int l) {
    return sequence(b, l + 1, op_parser_);
  }

  /* ********************************************************** */
  // vars '::' ctype
  static boolean gendeclfst(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "gendeclfst")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = vars(b, l + 1);
    r = r && consumeToken(b, DOUBLECOLON);
    p = r; // pin = 2
    r = r && ctype(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // pat '<-' exp
  //         | "let" decls
  //         | infixexp
  public static boolean guard(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guard")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<guard>");
    r = guard_0(b, l + 1);
    if (!r) r = guard_1(b, l + 1);
    if (!r) r = infixexp(b, l + 1);
    exit_section_(b, l, m, GUARD, r, false, null);
    return r;
  }

  // pat '<-' exp
  private static boolean guard_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guard_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pat(b, l + 1);
    r = r && consumeToken(b, LEFTARROW);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "let" decls
  private static boolean guard_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guard_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LET);
    r = r && decls(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '|' guard (',' guard)*
  static boolean guards(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guards")) return false;
    if (!nextTokenIs(b, PIPE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, PIPE);
    p = r; // pin = 1
    r = r && report_error_(b, guard(b, l + 1));
    r = p && guards_2(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' guard)*
  private static boolean guards_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guards_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!guards_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "guards_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' guard
  private static boolean guards_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guards_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && guard(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<indented false>>
  static boolean i(PsiBuilder b, int l) {
    return indented(b, l + 1, false);
  }

  /* ********************************************************** */
  // open (alt|exp) (semi alt)* close?
  static boolean iAlts(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "iAlts")) return false;
    if (!nextTokenIs(b, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && iAlts_1(b, l + 1);
    r = r && iAlts_2(b, l + 1);
    r = r && iAlts_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // alt|exp
  private static boolean iAlts_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "iAlts_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = alt(b, l + 1);
    if (!r) r = exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (semi alt)*
  private static boolean iAlts_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "iAlts_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!iAlts_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "iAlts_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // semi alt
  private static boolean iAlts_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "iAlts_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = semi(b, l + 1);
    r = r && alt(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // close?
  private static boolean iAlts_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "iAlts_3")) return false;
    close(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // itdecl
  //         | funorpatdecl
  public static boolean idecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecl")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<idecl>");
    r = itdecl(b, l + 1);
    if (!r) r = funorpatdecl(b, l + 1);
    exit_section_(b, l, m, IDECL, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // ppragma* idecl ppragma*
  static boolean idecl0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecl0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = idecl0_0(b, l + 1);
    r = r && idecl(b, l + 1);
    r = r && idecl0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ppragma*
  private static boolean idecl0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecl0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "idecl0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ppragma*
  private static boolean idecl0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecl0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "idecl0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // ppragma* open [idecls1] close
  static boolean idecls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecls")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = idecls_0(b, l + 1);
    r = r && open(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, idecls_2(b, l + 1));
    r = p && close(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ppragma*
  private static boolean idecls_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecls_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "idecls_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // [idecls1]
  private static boolean idecls_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecls_2")) return false;
    idecls1(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // idecl0 [semi idecls1]
  static boolean idecls1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecls1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = idecl0(b, l + 1);
    r = r && idecls1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [semi idecls1]
  private static boolean idecls1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecls1_1")) return false;
    idecls1_1_0(b, l + 1);
    return true;
  }

  // semi idecls1
  private static boolean idecls1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idecls1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = semi(b, l + 1);
    r = r && idecls1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "import" ["qualified"] qconid ["as" qconid] [impspec]
  public static boolean impdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecl")) return false;
    if (!nextTokenIs(b, IMPORT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, IMPORT);
    p = r; // pin = 1
    r = r && report_error_(b, impdecl_1(b, l + 1));
    r = p && report_error_(b, qconid(b, l + 1)) && r;
    r = p && report_error_(b, impdecl_3(b, l + 1)) && r;
    r = p && impdecl_4(b, l + 1) && r;
    exit_section_(b, l, m, IMPDECL, r, p, null);
    return r || p;
  }

  // ["qualified"]
  private static boolean impdecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecl_1")) return false;
    consumeToken(b, QUALIFIED);
    return true;
  }

  // ["as" qconid]
  private static boolean impdecl_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecl_3")) return false;
    impdecl_3_0(b, l + 1);
    return true;
  }

  // "as" qconid
  private static boolean impdecl_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecl_3_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, AS);
    p = r; // pin = 1
    r = r && qconid(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [impspec]
  private static boolean impdecl_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecl_4")) return false;
    impspec(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // impdecl [semi impdecls]
  static boolean impdecls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecls")) return false;
    if (!nextTokenIs(b, IMPORT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = impdecl(b, l + 1);
    p = r; // pin = 1
    r = r && impdecls_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [semi impdecls]
  private static boolean impdecls_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecls_1")) return false;
    impdecls_1_0(b, l + 1);
    return true;
  }

  // semi impdecls
  private static boolean impdecls_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impdecls_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = semi(b, l + 1);
    r = r && impdecls(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "()"
  public static boolean impempty(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impempty")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<impempty>");
    r = consumeToken(b, "()");
    exit_section_(b, l, m, IMPEMPTY, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '\"' "wrapper" '\"'
  //                  | '\"' "dynamic" '\"'
  //                  | pstringtoken
  static boolean impent(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impent")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = impent_0(b, l + 1);
    if (!r) r = impent_1(b, l + 1);
    if (!r) r = pstringtoken(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '\"' "wrapper" '\"'
  private static boolean impent_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impent_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "\\\"");
    r = r && consumeToken(b, "wrapper");
    r = r && consumeToken(b, "\\\"");
    exit_section_(b, m, null, r);
    return r;
  }

  // '\"' "dynamic" '\"'
  private static boolean impent_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impent_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "\\\"");
    r = r && consumeToken(b, "dynamic");
    r = r && consumeToken(b, "\\\"");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "import" callconv [safety] impent
  static boolean importdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "importdecl")) return false;
    if (!nextTokenIs(b, IMPORT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, IMPORT);
    p = r; // pin = 1
    r = r && report_error_(b, callconv(b, l + 1));
    r = p && report_error_(b, importdecl_2(b, l + 1)) && r;
    r = p && impent(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [safety]
  private static boolean importdecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "importdecl_2")) return false;
    safety(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // var
  //           // Really (tycon|tycls), but they are both ::= conid.
  //           | tycon ['(' (".." | cnames | vars) ')']
  public static boolean importt(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "importt")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<importt>");
    r = var(b, l + 1);
    if (!r) r = importt_1(b, l + 1);
    exit_section_(b, l, m, IMPORTT, r, false, null);
    return r;
  }

  // tycon ['(' (".." | cnames | vars) ')']
  private static boolean importt_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "importt_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tycon(b, l + 1);
    r = r && importt_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['(' (".." | cnames | vars) ')']
  private static boolean importt_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "importt_1_1")) return false;
    importt_1_1_0(b, l + 1);
    return true;
  }

  // '(' (".." | cnames | vars) ')'
  private static boolean importt_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "importt_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && importt_1_1_0_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // ".." | cnames | vars
  private static boolean importt_1_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "importt_1_1_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLEPERIOD);
    if (!r) r = cnames(b, l + 1);
    if (!r) r = vars(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // [i "hiding"] '(' [<<sequence importt>>] ')' | impempty
  static boolean impspec(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impspec")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = impspec_0(b, l + 1);
    if (!r) r = impempty(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [i "hiding"] '(' [<<sequence importt>>] ')'
  private static boolean impspec_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impspec_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = impspec_0_0(b, l + 1);
    r = r && consumeToken(b, LPAREN);
    p = r; // pin = 2
    r = r && report_error_(b, impspec_0_2(b, l + 1));
    r = p && consumeToken(b, RPAREN) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [i "hiding"]
  private static boolean impspec_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impspec_0_0")) return false;
    impspec_0_0_0(b, l + 1);
    return true;
  }

  // i "hiding"
  private static boolean impspec_0_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impspec_0_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = i(b, l + 1);
    r = r && consumeToken(b, HIDING);
    exit_section_(b, m, null, r);
    return r;
  }

  // [<<sequence importt>>]
  private static boolean impspec_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "impspec_0_2")) return false;
    sequence(b, l + 1, importt_parser_);
    return true;
  }

  /* ********************************************************** */
  // '-'* lexp [qop infixexp]
  static boolean infixexp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infixexp")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = infixexp_0(b, l + 1);
    r = r && lexp(b, l + 1);
    r = r && infixexp_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '-'*
  private static boolean infixexp_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infixexp_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, MINUS)) break;
      if (!empty_element_parsed_guard_(b, "infixexp_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // [qop infixexp]
  private static boolean infixexp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infixexp_2")) return false;
    infixexp_2_0(b, l + 1);
    return true;
  }

  // qop infixexp
  private static boolean infixexp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infixexp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qop(b, l + 1);
    r = r && infixexp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "instance" ctype ["where" idecls]
  public static boolean instancedecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "instancedecl")) return false;
    if (!nextTokenIs(b, INSTANCE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, INSTANCE);
    p = r; // pin = 1
    r = r && report_error_(b, ctype(b, l + 1));
    r = p && instancedecl_2(b, l + 1) && r;
    exit_section_(b, l, m, INSTANCEDECL, r, p, null);
    return r || p;
  }

  // ["where" idecls]
  private static boolean instancedecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "instancedecl_2")) return false;
    instancedecl_2_0(b, l + 1);
    return true;
  }

  // "where" idecls
  private static boolean instancedecl_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "instancedecl_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, WHERE);
    r = r && idecls(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "type" ctype '=' ctype
  //                  | ("data" | "newtype") ctype ([kindsig] gadtconstrs | ['=' constrs]) [deriving]
  static boolean itdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = itdecl_0(b, l + 1);
    if (!r) r = itdecl_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "type" ctype '=' ctype
  private static boolean itdecl_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, TYPE);
    r = r && ctype(b, l + 1);
    r = r && consumeToken(b, EQUALS);
    r = r && ctype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ("data" | "newtype") ctype ([kindsig] gadtconstrs | ['=' constrs]) [deriving]
  private static boolean itdecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = itdecl_1_0(b, l + 1);
    r = r && ctype(b, l + 1);
    r = r && itdecl_1_2(b, l + 1);
    r = r && itdecl_1_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "data" | "newtype"
  private static boolean itdecl_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DATA);
    if (!r) r = consumeToken(b, NEWTYPE);
    exit_section_(b, m, null, r);
    return r;
  }

  // [kindsig] gadtconstrs | ['=' constrs]
  private static boolean itdecl_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = itdecl_1_2_0(b, l + 1);
    if (!r) r = itdecl_1_2_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [kindsig] gadtconstrs
  private static boolean itdecl_1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = itdecl_1_2_0_0(b, l + 1);
    r = r && gadtconstrs(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [kindsig]
  private static boolean itdecl_1_2_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1_2_0_0")) return false;
    kindsig(b, l + 1);
    return true;
  }

  // ['=' constrs]
  private static boolean itdecl_1_2_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1_2_1")) return false;
    itdecl_1_2_1_0(b, l + 1);
    return true;
  }

  // '=' constrs
  private static boolean itdecl_1_2_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1_2_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, EQUALS);
    r = r && constrs(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [deriving]
  private static boolean itdecl_1_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "itdecl_1_3")) return false;
    deriving(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // bkind ['->' kind]
  public static boolean kind(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kind")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<kind>");
    r = bkind(b, l + 1);
    r = r && kind_1(b, l + 1);
    exit_section_(b, l, m, KIND, r, false, null);
    return r;
  }

  // ['->' kind]
  private static boolean kind_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kind_1")) return false;
    kind_1_0(b, l + 1);
    return true;
  }

  // '->' kind
  private static boolean kind_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kind_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, RIGHTARROW);
    r = r && kind(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // typee (typee | '(' tyvar kindsig ')')* [kindsig]
  static boolean kindedvars(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kindedvars")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typee(b, l + 1);
    r = r && kindedvars_1(b, l + 1);
    r = r && kindedvars_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (typee | '(' tyvar kindsig ')')*
  private static boolean kindedvars_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kindedvars_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!kindedvars_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "kindedvars_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // typee | '(' tyvar kindsig ')'
  private static boolean kindedvars_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kindedvars_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typee(b, l + 1);
    if (!r) r = kindedvars_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' tyvar kindsig ')'
  private static boolean kindedvars_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kindedvars_1_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && tyvar(b, l + 1);
    r = r && kindsig(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // [kindsig]
  private static boolean kindedvars_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kindedvars_2")) return false;
    kindsig(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // "::" kind
  static boolean kindsig(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "kindsig")) return false;
    if (!nextTokenIs(b, DOUBLECOLON)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, DOUBLECOLON);
    p = r; // pin = 1
    r = r && kind(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // "let" decls "in" exp
  public static boolean letexp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "letexp")) return false;
    if (!nextTokenIs(b, LET)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LET);
    r = r && decls(b, l + 1);
    r = r && consumeToken(b, IN);
    r = r && exp(b, l + 1);
    exit_section_(b, m, LETEXP, r);
    return r;
  }

  /* ********************************************************** */
  // "\\case" altslist
  //                | '\' (apat | thaexp)+ "->" exp
  //                | letexp
  //                | "if" exp [semi] "then" exp [semi] "else" exp
  //                | "case" exp "of" altslist
  //                | "do" open stmts close
  //                | "mdo" open stmts close
  //                // proc might just be a variable name.
  //                | "proc" (aexp "->" exp | aexp*)
  //                | aexp+
  static boolean lexp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = lexp_0(b, l + 1);
    if (!r) r = lexp_1(b, l + 1);
    if (!r) r = letexp(b, l + 1);
    if (!r) r = lexp_3(b, l + 1);
    if (!r) r = lexp_4(b, l + 1);
    if (!r) r = lexp_5(b, l + 1);
    if (!r) r = lexp_6(b, l + 1);
    if (!r) r = lexp_7(b, l + 1);
    if (!r) r = lexp_8(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "\\case" altslist
  private static boolean lexp_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "\\case");
    r = r && altslist(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '\' (apat | thaexp)+ "->" exp
  private static boolean lexp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, BACKSLASH);
    r = r && lexp_1_1(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, RIGHTARROW));
    r = p && exp(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (apat | thaexp)+
  private static boolean lexp_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = lexp_1_1_0(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!lexp_1_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "lexp_1_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // apat | thaexp
  private static boolean lexp_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat(b, l + 1);
    if (!r) r = thaexp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "if" exp [semi] "then" exp [semi] "else" exp
  private static boolean lexp_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_3")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, IF);
    r = r && exp(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, lexp_3_2(b, l + 1));
    r = p && report_error_(b, consumeToken(b, THEN)) && r;
    r = p && report_error_(b, exp(b, l + 1)) && r;
    r = p && report_error_(b, lexp_3_5(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, ELSE)) && r;
    r = p && exp(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [semi]
  private static boolean lexp_3_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_3_2")) return false;
    semi(b, l + 1);
    return true;
  }

  // [semi]
  private static boolean lexp_3_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_3_5")) return false;
    semi(b, l + 1);
    return true;
  }

  // "case" exp "of" altslist
  private static boolean lexp_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_4")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, CASE);
    r = r && exp(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, OF));
    r = p && altslist(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // "do" open stmts close
  private static boolean lexp_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_5")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, DO);
    r = r && open(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, stmts(b, l + 1));
    r = p && close(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // "mdo" open stmts close
  private static boolean lexp_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_6")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, MDOTOK);
    r = r && open(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, stmts(b, l + 1));
    r = p && close(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // "proc" (aexp "->" exp | aexp*)
  private static boolean lexp_7(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_7")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "proc");
    r = r && lexp_7_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // aexp "->" exp | aexp*
  private static boolean lexp_7_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_7_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = lexp_7_1_0(b, l + 1);
    if (!r) r = lexp_7_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // aexp "->" exp
  private static boolean lexp_7_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_7_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = aexp(b, l + 1);
    r = r && consumeToken(b, RIGHTARROW);
    p = r; // pin = 2
    r = r && exp(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // aexp*
  private static boolean lexp_7_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_7_1_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!aexp(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "lexp_7_1_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // aexp+
  private static boolean lexp_8(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lexp_8")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = aexp(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!aexp(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "lexp_8", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '[' exp [listlike1] ']'
  static boolean listlike(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike")) return false;
    if (!nextTokenIs(b, LBRACKET)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACKET);
    r = r && exp(b, l + 1);
    r = r && listlike_2(b, l + 1);
    r = r && consumeToken(b, RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // [listlike1]
  private static boolean listlike_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike_2")) return false;
    listlike1(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // ('|' (squal ',')* squal)+
  //                     | [',' exp] '..' [exp]
  //                     | (',' exp)+
  static boolean listlike1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = listlike1_0(b, l + 1);
    if (!r) r = listlike1_1(b, l + 1);
    if (!r) r = listlike1_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ('|' (squal ',')* squal)+
  private static boolean listlike1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = listlike1_0_0(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!listlike1_0_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "listlike1_0", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // '|' (squal ',')* squal
  private static boolean listlike1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PIPE);
    r = r && listlike1_0_0_1(b, l + 1);
    r = r && squal(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (squal ',')*
  private static boolean listlike1_0_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_0_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!listlike1_0_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "listlike1_0_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // squal ','
  private static boolean listlike1_0_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_0_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = squal(b, l + 1);
    r = r && consumeToken(b, COMMA);
    exit_section_(b, m, null, r);
    return r;
  }

  // [',' exp] '..' [exp]
  private static boolean listlike1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = listlike1_1_0(b, l + 1);
    r = r && consumeToken(b, DOUBLEPERIOD);
    r = r && listlike1_1_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [',' exp]
  private static boolean listlike1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_1_0")) return false;
    listlike1_1_0_0(b, l + 1);
    return true;
  }

  // ',' exp
  private static boolean listlike1_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [exp]
  private static boolean listlike1_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_1_2")) return false;
    exp(b, l + 1);
    return true;
  }

  // (',' exp)+
  private static boolean listlike1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = listlike1_2_0(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!listlike1_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "listlike1_2", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' exp
  private static boolean listlike1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "listlike1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // floattoken | integertoken | chartoken | pstringtoken ['#']
  static boolean literal(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "literal")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, FLOATTOKEN);
    if (!r) r = consumeToken(b, INTEGERTOKEN);
    if (!r) r = consumeToken(b, CHARTOKEN);
    if (!r) r = literal_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // pstringtoken ['#']
  private static boolean literal_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "literal_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pstringtoken(b, l + 1);
    r = r && literal_3_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['#']
  private static boolean literal_3_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "literal_3_1")) return false;
    consumeToken(b, HASH);
    return true;
  }

  /* ********************************************************** */
  // '-' (integertoken|floattoken)
  //                | gcon apat+
  //                | apat
  static boolean lpat(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lpat")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = lpat_0(b, l + 1);
    if (!r) r = lpat_1(b, l + 1);
    if (!r) r = apat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '-' (integertoken|floattoken)
  private static boolean lpat_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lpat_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, MINUS);
    r = r && lpat_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // integertoken|floattoken
  private static boolean lpat_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lpat_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, INTEGERTOKEN);
    if (!r) r = consumeToken(b, FLOATTOKEN);
    exit_section_(b, m, null, r);
    return r;
  }

  // gcon apat+
  private static boolean lpat_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lpat_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = gcon(b, l + 1);
    r = r && lpat_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // apat+
  private static boolean lpat_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lpat_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = apat(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!apat(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "lpat_1_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // shebang? ppragma* [moduledecl] body
  static boolean module(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = module_0(b, l + 1);
    r = r && module_1(b, l + 1);
    r = r && module_2(b, l + 1);
    r = r && body(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // shebang?
  private static boolean module_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_0")) return false;
    shebang(b, l + 1);
    return true;
  }

  // ppragma*
  private static boolean module_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "module_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // [moduledecl]
  private static boolean module_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_2")) return false;
    moduledecl(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // (conid '.')+
  static boolean modulePrefix(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "modulePrefix")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = modulePrefix_0(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!modulePrefix_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "modulePrefix", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // conid '.'
  private static boolean modulePrefix_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "modulePrefix_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = conid(b, l + 1);
    r = r && consumeToken(b, PERIOD);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "module" qconid [ppragma] [exportsempty|exports] "where"
  public static boolean moduledecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "moduledecl")) return false;
    if (!nextTokenIs(b, MODULETOKEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, MODULETOKEN);
    p = r; // pin = 1
    r = r && report_error_(b, qconid(b, l + 1));
    r = p && report_error_(b, moduledecl_2(b, l + 1)) && r;
    r = p && report_error_(b, moduledecl_3(b, l + 1)) && r;
    r = p && consumeToken(b, WHERE) && r;
    exit_section_(b, l, m, MODULEDECL, r, p, null);
    return r || p;
  }

  // [ppragma]
  private static boolean moduledecl_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "moduledecl_2")) return false;
    ppragma(b, l + 1);
    return true;
  }

  // [exportsempty|exports]
  private static boolean moduledecl_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "moduledecl_3")) return false;
    moduledecl_3_0(b, l + 1);
    return true;
  }

  // exportsempty|exports
  private static boolean moduledecl_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "moduledecl_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exportsempty(b, l + 1);
    if (!r) r = exports(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // con (atype | '{' var '::' typee '}')
  public static boolean newconstr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "newconstr")) return false;
    if (!nextTokenIs(b, "<newconstr>", LPAREN, CONIDREGEXP)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<newconstr>");
    r = con(b, l + 1);
    p = r; // pin = 1
    r = r && newconstr_1(b, l + 1);
    exit_section_(b, l, m, NEWCONSTR, r, p, null);
    return r || p;
  }

  // atype | '{' var '::' typee '}'
  private static boolean newconstr_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "newconstr_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atype(b, l + 1);
    if (!r) r = newconstr_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '{' var '::' typee '}'
  private static boolean newconstr_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "newconstr_1_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, LBRACE);
    p = r; // pin = 1
    r = r && report_error_(b, var(b, l + 1));
    r = p && report_error_(b, consumeToken(b, DOUBLECOLON)) && r;
    r = p && report_error_(b, typee(b, l + 1)) && r;
    r = p && consumeToken(b, RBRACE) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // "newtype" [context "=>"] simpletype '=' newconstr [deriving]
  public static boolean newtypedecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "newtypedecl")) return false;
    if (!nextTokenIs(b, NEWTYPE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, NEWTYPE);
    p = r; // pin = 1
    r = r && report_error_(b, newtypedecl_1(b, l + 1));
    r = p && report_error_(b, simpletype(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, EQUALS)) && r;
    r = p && report_error_(b, newconstr(b, l + 1)) && r;
    r = p && newtypedecl_5(b, l + 1) && r;
    exit_section_(b, l, m, NEWTYPEDECL, r, p, null);
    return r || p;
  }

  // [context "=>"]
  private static boolean newtypedecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "newtypedecl_1")) return false;
    newtypedecl_1_0(b, l + 1);
    return true;
  }

  // context "=>"
  private static boolean newtypedecl_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "newtypedecl_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = context(b, l + 1);
    r = r && consumeToken(b, DOUBLEARROW);
    exit_section_(b, m, null, r);
    return r;
  }

  // [deriving]
  private static boolean newtypedecl_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "newtypedecl_5")) return false;
    deriving(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '(#' commas '#)'
  //                    | '(' ('->' | commas) ')'
  // //                   | '[:' ':]'
  //                    | '[' ']'
  // //                   | '(' '~#' ')'
  //                    | oqtycon
  static boolean ntgtycon(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ntgtycon")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = ntgtycon_0(b, l + 1);
    if (!r) r = ntgtycon_1(b, l + 1);
    if (!r) r = ntgtycon_2(b, l + 1);
    if (!r) r = oqtycon(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(#' commas '#)'
  private static boolean ntgtycon_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ntgtycon_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LUNBOXPAREN);
    r = r && commas(b, l + 1);
    r = r && consumeToken(b, RUNBOXPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' ('->' | commas) ')'
  private static boolean ntgtycon_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ntgtycon_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && ntgtycon_1_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // '->' | commas
  private static boolean ntgtycon_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ntgtycon_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, RIGHTARROW);
    if (!r) r = commas(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '[' ']'
  private static boolean ntgtycon_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ntgtycon_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACKET);
    r = r && consumeToken(b, RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // varop | conop
  public static boolean op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "op")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<op>");
    r = varop(b, l + 1);
    if (!r) r = conop(b, l + 1);
    exit_section_(b, l, m, OP, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '{'
  //                 | WHITESPACELBRACETOK
  static boolean open(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "open")) return false;
    if (!nextTokenIs(b, "", LBRACE, WHITESPACELBRACETOK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACE);
    if (!r) r = consumeToken(b, WHITESPACELBRACETOK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' ('~' | qtyconsym) ')'        // An "ordinary" qualified tycon;
  //           | qtycon
  public static boolean oqtycon(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "oqtycon")) return false;
    if (!nextTokenIs(b, "<oqtycon>", LPAREN, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<oqtycon>");
    r = oqtycon_0(b, l + 1);
    if (!r) r = qtycon(b, l + 1);
    exit_section_(b, l, m, OQTYCON, r, false, null);
    return r;
  }

  // '(' ('~' | qtyconsym) ')'
  private static boolean oqtycon_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "oqtycon_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && oqtycon_0_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // '~' | qtyconsym
  private static boolean oqtycon_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "oqtycon_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, TILDE);
    if (!r) r = qtyconsym(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' parenlike1
  static boolean parenlike(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike")) return false;
    if (!nextTokenIs(b, LPAREN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && parenlike1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // exp (',' exp)* ')'
  //                      | (infixexp qop | qop infixexp) ')'
  static boolean parenlike1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = parenlike1_0(b, l + 1);
    if (!r) r = parenlike1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // exp (',' exp)* ')'
  private static boolean parenlike1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && parenlike1_0_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' exp)*
  private static boolean parenlike1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!parenlike1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "parenlike1_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' exp
  private static boolean parenlike1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (infixexp qop | qop infixexp) ')'
  private static boolean parenlike1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = parenlike1_1_0(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // infixexp qop | qop infixexp
  private static boolean parenlike1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = parenlike1_1_0_0(b, l + 1);
    if (!r) r = parenlike1_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // infixexp qop
  private static boolean parenlike1_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1_1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = infixexp(b, l + 1);
    r = r && qop(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // qop infixexp
  private static boolean parenlike1_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenlike1_1_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qop(b, l + 1);
    r = r && infixexp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ["rec"] "let" decls
  //                       | [pat '<-'] exp
  static boolean partialstmt(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "partialstmt")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = partialstmt_0(b, l + 1);
    if (!r) r = partialstmt_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["rec"] "let" decls
  private static boolean partialstmt_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "partialstmt_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = partialstmt_0_0(b, l + 1);
    r = r && consumeToken(b, LET);
    r = r && decls(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["rec"]
  private static boolean partialstmt_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "partialstmt_0_0")) return false;
    consumeToken(b, RECTOK);
    return true;
  }

  // [pat '<-'] exp
  private static boolean partialstmt_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "partialstmt_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = partialstmt_1_0(b, l + 1);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [pat '<-']
  private static boolean partialstmt_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "partialstmt_1_0")) return false;
    partialstmt_1_0_0(b, l + 1);
    return true;
  }

  // pat '<-'
  private static boolean partialstmt_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "partialstmt_1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pat(b, l + 1);
    r = r && consumeToken(b, LEFTARROW);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // lpat (qconop pat | ["::" ctype])
  public static boolean pat(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pat")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<pat>");
    r = lpat(b, l + 1);
    r = r && pat_1(b, l + 1);
    exit_section_(b, l, m, PAT, r, false, null);
    return r;
  }

  // qconop pat | ["::" ctype]
  private static boolean pat_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pat_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pat_1_0(b, l + 1);
    if (!r) r = pat_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // qconop pat
  private static boolean pat_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pat_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qconop(b, l + 1);
    r = r && pat(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["::" ctype]
  private static boolean pat_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pat_1_1")) return false;
    pat_1_1_0(b, l + 1);
    return true;
  }

  // "::" ctype
  private static boolean pat_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pat_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLECOLON);
    r = r && ctype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // qtycon
  //                 | '(' [kind ',' comma_kinds1] ')'
  //                 | '[' kind [',' comma_kinds1] ']'
  static boolean pkind(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkind")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qtycon(b, l + 1);
    if (!r) r = pkind_1(b, l + 1);
    if (!r) r = pkind_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' [kind ',' comma_kinds1] ')'
  private static boolean pkind_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkind_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && pkind_1_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  // [kind ',' comma_kinds1]
  private static boolean pkind_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkind_1_1")) return false;
    pkind_1_1_0(b, l + 1);
    return true;
  }

  // kind ',' comma_kinds1
  private static boolean pkind_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkind_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = kind(b, l + 1);
    r = r && consumeToken(b, COMMA);
    r = r && comma_kinds1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '[' kind [',' comma_kinds1] ']'
  private static boolean pkind_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkind_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACKET);
    r = r && kind(b, l + 1);
    r = r && pkind_2_2(b, l + 1);
    r = r && consumeToken(b, RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // [',' comma_kinds1]
  private static boolean pkind_2_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkind_2_2")) return false;
    pkind_2_2_0(b, l + 1);
    return true;
  }

  // ',' comma_kinds1
  private static boolean pkind_2_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkind_2_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && comma_kinds1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // openpragma PRAGMA+ closepragma
  public static boolean ppragma(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ppragma")) return false;
    if (!nextTokenIs(b, OPENPRAGMA)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, OPENPRAGMA);
    r = r && ppragma_1(b, l + 1);
    r = r && consumeToken(b, CLOSEPRAGMA);
    exit_section_(b, m, PPRAGMA, r);
    return r;
  }

  // PRAGMA+
  private static boolean ppragma_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ppragma_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PRAGMA);
    int c = current_position_(b);
    while (r) {
      if (!consumeToken(b, PRAGMA)) break;
      if (!empty_element_parsed_guard_(b, "ppragma_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '"' STRINGTOKEN* '"'
  public static boolean pstringtoken(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pstringtoken")) return false;
    if (!nextTokenIs(b, DOUBLEQUOTE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLEQUOTE);
    r = r && pstringtoken_1(b, l + 1);
    r = r && consumeToken(b, DOUBLEQUOTE);
    exit_section_(b, m, PSTRINGTOKEN, r);
    return r;
  }

  // STRINGTOKEN*
  private static boolean pstringtoken_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pstringtoken_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, STRINGTOKEN)) break;
      if (!empty_element_parsed_guard_(b, "pstringtoken_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // qconid | '(' gconsym ')'
  public static boolean qcon(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qcon")) return false;
    if (!nextTokenIs(b, "<qcon>", LPAREN, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qcon>");
    r = qconid(b, l + 1);
    if (!r) r = qcon_1(b, l + 1);
    exit_section_(b, l, m, QCON, r, false, null);
    return r;
  }

  // '(' gconsym ')'
  private static boolean qcon_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qcon_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && gconsym(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // [modulePrefix] conid ['#']
  public static boolean qconid(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qconid")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qconid_0(b, l + 1);
    r = r && conid(b, l + 1);
    r = r && qconid_2(b, l + 1);
    exit_section_(b, m, QCONID, r);
    return r;
  }

  // [modulePrefix]
  private static boolean qconid_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qconid_0")) return false;
    modulePrefix(b, l + 1);
    return true;
  }

  // ['#']
  private static boolean qconid_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qconid_2")) return false;
    consumeToken(b, HASH);
    return true;
  }

  /* ********************************************************** */
  // gconsym | '`' qconid '`'
  public static boolean qconop(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qconop")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qconop>");
    r = gconsym(b, l + 1);
    if (!r) r = qconop_1(b, l + 1);
    exit_section_(b, l, m, QCONOP, r, false, null);
    return r;
  }

  // '`' qconid '`'
  private static boolean qconop_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qconop_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BACKTICK);
    r = r && qconid(b, l + 1);
    r = r && consumeToken(b, BACKTICK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // [modulePrefix] consym
  public static boolean qconsym(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qconsym")) return false;
    if (!nextTokenIs(b, "<qconsym>", CONSYMTOK, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qconsym>");
    r = qconsym_0(b, l + 1);
    r = r && consym(b, l + 1);
    exit_section_(b, l, m, QCONSYM, r, false, null);
    return r;
  }

  // [modulePrefix]
  private static boolean qconsym_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qconsym_0")) return false;
    modulePrefix(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // qvarop | qconop
  public static boolean qop(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qop")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qop>");
    r = qvarop(b, l + 1);
    if (!r) r = qconop(b, l + 1);
    exit_section_(b, l, m, QOP, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // qqtext+
  public static boolean qqblob(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qqblob")) return false;
    if (!nextTokenIs(b, QQTEXT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, QQTEXT);
    int c = current_position_(b);
    while (r) {
      if (!consumeToken(b, QQTEXT)) break;
      if (!empty_element_parsed_guard_(b, "qqblob", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, QQBLOB, r);
    return r;
  }

  /* ********************************************************** */
  // [modulePrefix] tycls
  public static boolean qtycls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qtycls")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qtycls_0(b, l + 1);
    r = r && tycls(b, l + 1);
    exit_section_(b, m, QTYCLS, r);
    return r;
  }

  // [modulePrefix]
  private static boolean qtycls_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qtycls_0")) return false;
    modulePrefix(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // [modulePrefix] tycon
  public static boolean qtycon(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qtycon")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qtycon_0(b, l + 1);
    r = r && tycon(b, l + 1);
    exit_section_(b, m, QTYCON, r);
    return r;
  }

  // [modulePrefix]
  private static boolean qtycon_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qtycon_0")) return false;
    modulePrefix(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // qtyconsym | '`' qtycon '`'
  public static boolean qtyconop(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qtyconop")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qtyconop>");
    r = qtyconsym(b, l + 1);
    if (!r) r = qtyconop_1(b, l + 1);
    exit_section_(b, l, m, QTYCONOP, r, false, null);
    return r;
  }

  // '`' qtycon '`'
  private static boolean qtyconop_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qtyconop_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BACKTICK);
    r = r && qtycon(b, l + 1);
    r = r && consumeToken(b, BACKTICK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // qconsym | qvarsym | tyconsym
  public static boolean qtyconsym(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qtyconsym")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qtyconsym>");
    r = qconsym(b, l + 1);
    if (!r) r = qvarsym(b, l + 1);
    if (!r) r = tyconsym(b, l + 1);
    exit_section_(b, l, m, QTYCONSYM, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // pat '<-' exp
  //               | "let" decls
  //               | exp
  static boolean qual(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qual")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qual_0(b, l + 1);
    if (!r) r = qual_1(b, l + 1);
    if (!r) r = exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // pat '<-' exp
  private static boolean qual_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qual_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = pat(b, l + 1);
    r = r && consumeToken(b, LEFTARROW);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // "let" decls
  private static boolean qual_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qual_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LET);
    r = r && decls(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // qvarid | '(' qvarsym ')'
  public static boolean qvar(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvar")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qvar>");
    r = qvarid(b, l + 1);
    if (!r) r = qvar_1(b, l + 1);
    exit_section_(b, l, m, QVAR, r, false, null);
    return r;
  }

  // '(' qvarsym ')'
  private static boolean qvar_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvar_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && qvarsym(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // [modulePrefix] varid
  public static boolean qvarid(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvarid")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qvarid>");
    r = qvarid_0(b, l + 1);
    r = r && varid(b, l + 1);
    exit_section_(b, l, m, QVARID, r, false, null);
    return r;
  }

  // [modulePrefix]
  private static boolean qvarid_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvarid_0")) return false;
    modulePrefix(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // qvarsym | '`' qvarid '`'
  public static boolean qvarop(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvarop")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qvarop>");
    r = qvarsym(b, l + 1);
    if (!r) r = qvarop_1(b, l + 1);
    exit_section_(b, l, m, QVAROP, r, false, null);
    return r;
  }

  // '`' qvarid '`'
  private static boolean qvarop_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvarop_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BACKTICK);
    r = r && qvarid(b, l + 1);
    r = r && consumeToken(b, BACKTICK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<sequence qvar>>
  public static boolean qvars(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvars")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qvars>");
    r = sequence(b, l + 1, qvar_parser_);
    exit_section_(b, l, m, QVARS, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // [modulePrefix] varsym
  public static boolean qvarsym(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvarsym")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<qvarsym>");
    r = qvarsym_0(b, l + 1);
    r = r && varsym(b, l + 1);
    exit_section_(b, l, m, QVARSYM, r, false, null);
    return r;
  }

  // [modulePrefix]
  private static boolean qvarsym_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qvarsym_0")) return false;
    modulePrefix(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // qvar
  //                         | qcon
  //                         | gcon
  //                         | '(' recordlikeparen ')'
  static boolean recordlikelhs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "recordlikelhs")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qvar(b, l + 1);
    if (!r) r = qcon(b, l + 1);
    if (!r) r = gcon(b, l + 1);
    if (!r) r = recordlikelhs_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' recordlikeparen ')'
  private static boolean recordlikelhs_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "recordlikelhs_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && recordlikeparen(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // exp
  //                           | infixexp qop
  //                           | qop infixexp
  static boolean recordlikeparen(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "recordlikeparen")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    if (!r) r = recordlikeparen_1(b, l + 1);
    if (!r) r = recordlikeparen_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // infixexp qop
  private static boolean recordlikeparen_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "recordlikeparen_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = infixexp(b, l + 1);
    r = r && qop(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // qop infixexp
  private static boolean recordlikeparen_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "recordlikeparen_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qop(b, l + 1);
    r = r && infixexp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ('=' exp | gdrhs+) [wheredecls]
  public static boolean rhs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rhs")) return false;
    if (!nextTokenIs(b, "<rhs>", EQUALS, PIPE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<rhs>");
    r = rhs_0(b, l + 1);
    p = r; // pin = 1
    r = r && rhs_1(b, l + 1);
    exit_section_(b, l, m, RHS, r, p, null);
    return r || p;
  }

  // '=' exp | gdrhs+
  private static boolean rhs_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rhs_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = rhs_0_0(b, l + 1);
    if (!r) r = rhs_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '=' exp
  private static boolean rhs_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rhs_0_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, EQUALS);
    p = r; // pin = 1
    r = r && exp(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // gdrhs+
  private static boolean rhs_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rhs_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = gdrhs(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!gdrhs(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "rhs_0_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // [wheredecls]
  private static boolean rhs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rhs_1")) return false;
    wheredecls(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // "unsafe" | "safe" | "interruptible"
  static boolean safety(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "safety")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "unsafe");
    if (!r) r = consumeToken(b, "safe");
    if (!r) r = consumeToken(b, "interruptible");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ';'
  //                | WHITESPACESEMITOK
  static boolean semi(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "semi")) return false;
    if (!nextTokenIs(b, "", SEMICOLON, WHITESPACESEMITOK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SEMICOLON);
    if (!r) r = consumeToken(b, WHITESPACESEMITOK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<p>> (',' <<p>>)*
  static boolean sequence(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "sequence")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = _p.parse(b, l);
    p = r; // pin = 1
    r = r && sequence_1(b, l + 1, _p);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' <<p>>)*
  private static boolean sequence_1(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "sequence_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!sequence_1_0(b, l + 1, _p)) break;
      if (!empty_element_parsed_guard_(b, "sequence_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' <<p>>
  private static boolean sequence_1_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "sequence_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, COMMA);
    p = r; // pin = 1
    r = r && _p.parse(b, l);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // SHEBANGSTART SHEBANGPATH?
  public static boolean shebang(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "shebang")) return false;
    if (!nextTokenIs(b, SHEBANGSTART)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SHEBANGSTART);
    r = r && shebang_1(b, l + 1);
    exit_section_(b, m, SHEBANG, r);
    return r;
  }

  // SHEBANGPATH?
  private static boolean shebang_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "shebang_1")) return false;
    consumeToken(b, SHEBANGPATH);
    return true;
  }

  /* ********************************************************** */
  // tycon tyvar*
  static boolean simpletype(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simpletype")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tycon(b, l + 1);
    r = r && simpletype_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // tyvar*
  private static boolean simpletype_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simpletype_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!tyvar(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "simpletype_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // transformqual
  //                 | qual
  static boolean squal(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "squal")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = transformqual(b, l + 1);
    if (!r) r = qual(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // partialstmt semi
  static boolean stmt(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stmt")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = partialstmt(b, l + 1);
    r = r && semi(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // stmt* (letexp | partialstmt | exp)
  public static boolean stmts(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stmts")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<stmts>");
    r = stmts_0(b, l + 1);
    r = r && stmts_1(b, l + 1);
    exit_section_(b, l, m, STMTS, r, false, null);
    return r;
  }

  // stmt*
  private static boolean stmts_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stmts_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!stmt(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "stmts_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // letexp | partialstmt | exp
  private static boolean stmts_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stmts_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = letexp(b, l + 1);
    if (!r) r = partialstmt(b, l + 1);
    if (!r) r = exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // [ppragma] '!'
  static boolean strict_mark(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "strict_mark")) return false;
    if (!nextTokenIs(b, "", EXCLAMATION, OPENPRAGMA)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = strict_mark_0(b, l + 1);
    r = r && consumeToken(b, EXCLAMATION);
    exit_section_(b, m, null, r);
    return r;
  }

  // [ppragma]
  private static boolean strict_mark_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "strict_mark_0")) return false;
    ppragma(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '>' | '?' | '@'
  //          | '\' | '^' | '-' | '~' | ':'
  static boolean symbol1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "symbol1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, EXCLAMATION);
    if (!r) r = consumeToken(b, HASH);
    if (!r) r = consumeToken(b, DOLLAR);
    if (!r) r = consumeToken(b, PERCENT);
    if (!r) r = consumeToken(b, AMPERSAND);
    if (!r) r = consumeToken(b, ASTERISK);
    if (!r) r = consumeToken(b, PLUS);
    if (!r) r = consumeToken(b, PERIOD);
    if (!r) r = consumeToken(b, SLASH);
    if (!r) r = consumeToken(b, LESSTHAN);
    if (!r) r = consumeToken(b, GREATERTHAN);
    if (!r) r = consumeToken(b, QUESTION);
    if (!r) r = consumeToken(b, AMPERSAT);
    if (!r) r = consumeToken(b, BACKSLASH);
    if (!r) r = consumeToken(b, CARET);
    if (!r) r = consumeToken(b, MINUS);
    if (!r) r = consumeToken(b, TILDE);
    if (!r) r = consumeToken(b, COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '[|' e (exp [semi])+ e '|]'
  //                  // TODO: Enable more precise TH parsing with t/p/d.
  // //                 | '[' ("t" '|' ctype | "p" '|' infixexp |  "d" '|' open topdecls close ) '|]'
  //                  | qqopen i qvarid i '|' qqblob '|]'
  static boolean thaexp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "thaexp")) return false;
    if (!nextTokenIs(b, "", LTHOPEN, QQOPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = thaexp_0(b, l + 1);
    if (!r) r = thaexp_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '[|' e (exp [semi])+ e '|]'
  private static boolean thaexp_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "thaexp_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LTHOPEN);
    r = r && e(b, l + 1);
    r = r && thaexp_0_2(b, l + 1);
    r = r && e(b, l + 1);
    r = r && consumeToken(b, RTHCLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (exp [semi])+
  private static boolean thaexp_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "thaexp_0_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = thaexp_0_2_0(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!thaexp_0_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "thaexp_0_2", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // exp [semi]
  private static boolean thaexp_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "thaexp_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && thaexp_0_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [semi]
  private static boolean thaexp_0_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "thaexp_0_2_0_1")) return false;
    semi(b, l + 1);
    return true;
  }

  // qqopen i qvarid i '|' qqblob '|]'
  private static boolean thaexp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "thaexp_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, QQOPEN);
    r = r && i(b, l + 1);
    r = r && qvarid(b, l + 1);
    r = r && i(b, l + 1);
    r = r && consumeToken(b, PIPE);
    r = r && qqblob(b, l + 1);
    r = r && consumeToken(b, RTHCLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // topdecl1 ppragma*
  static boolean topdecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "topdecl")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = topdecl1(b, l + 1);
    r = r && topdecl_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ppragma*
  private static boolean topdecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "topdecl_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ppragma(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "topdecl_1", c)) break;
      c = current_position_(b);
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
  //                   | impdecl // From #ifdefs
  //                   | infixexp
  static boolean topdecl1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "topdecl1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typedecl(b, l + 1);
    if (!r) r = datadecl(b, l + 1);
    if (!r) r = newtypedecl(b, l + 1);
    if (!r) r = classdecl(b, l + 1);
    if (!r) r = instancedecl(b, l + 1);
    if (!r) r = defaultdecl(b, l + 1);
    if (!r) r = foreigndecl(b, l + 1);
    if (!r) r = derivingdecl(b, l + 1);
    if (!r) r = decl(b, l + 1);
    if (!r) r = impdecl(b, l + 1);
    if (!r) r = infixexp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // topdecl [semi topdecls]
  static boolean topdecls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "topdecls")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = topdecl(b, l + 1);
    p = r; // pin = 1
    r = r && topdecls_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [semi topdecls]
  private static boolean topdecls_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "topdecls_1")) return false;
    topdecls_1_0(b, l + 1);
    return true;
  }

  // semi topdecls
  private static boolean topdecls_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "topdecls_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = semi(b, l + 1);
    p = r; // pin = 1
    r = r && topdecls(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'then' ('group' ['by' exp] 'using' exp | exp ['by' exp])
  static boolean transformqual(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual")) return false;
    if (!nextTokenIs(b, THEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, THEN);
    r = r && transformqual_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // 'group' ['by' exp] 'using' exp | exp ['by' exp]
  private static boolean transformqual_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = transformqual_1_0(b, l + 1);
    if (!r) r = transformqual_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // 'group' ['by' exp] 'using' exp
  private static boolean transformqual_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "group");
    r = r && transformqual_1_0_1(b, l + 1);
    r = r && consumeToken(b, "using");
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['by' exp]
  private static boolean transformqual_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual_1_0_1")) return false;
    transformqual_1_0_1_0(b, l + 1);
    return true;
  }

  // 'by' exp
  private static boolean transformqual_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "by");
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // exp ['by' exp]
  private static boolean transformqual_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && transformqual_1_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['by' exp]
  private static boolean transformqual_1_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual_1_1_1")) return false;
    transformqual_1_1_1_0(b, l + 1);
    return true;
  }

  // 'by' exp
  private static boolean transformqual_1_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "transformqual_1_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "by");
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' tyvar kindsig ')'
  //           | tyvar
  public static boolean tv_bndr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tv_bndr")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<tv bndr>");
    r = tv_bndr_0(b, l + 1);
    if (!r) r = tyvar(b, l + 1);
    exit_section_(b, l, m, TV_BNDR, r, false, null);
    return r;
  }

  // '(' tyvar kindsig ')'
  private static boolean tv_bndr_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tv_bndr_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && tyvar(b, l + 1);
    r = r && kindsig(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // conid
  public static boolean tycls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tycls")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = conid(b, l + 1);
    exit_section_(b, m, TYCLS, r);
    return r;
  }

  /* ********************************************************** */
  // conid
  public static boolean tycon(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tycon")) return false;
    if (!nextTokenIs(b, CONIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = conid(b, l + 1);
    exit_section_(b, m, TYCON, r);
    return r;
  }

  /* ********************************************************** */
  // consym | varsym | '*' | '-'
  public static boolean tyconsym(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tyconsym")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<tyconsym>");
    r = consym(b, l + 1);
    if (!r) r = varsym(b, l + 1);
    if (!r) r = consumeToken(b, ASTERISK);
    if (!r) r = consumeToken(b, MINUS);
    exit_section_(b, l, m, TYCONSYM, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // "type" (typedeclclosedfamily | typedeclnorm)
  public static boolean typedecl(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedecl")) return false;
    if (!nextTokenIs(b, TYPE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, TYPE);
    p = r; // pin = 1
    r = r && typedecl_1(b, l + 1);
    exit_section_(b, l, m, TYPEDECL, r, p, null);
    return r || p;
  }

  // typedeclclosedfamily | typedeclnorm
  private static boolean typedecl_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedecl_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typedeclclosedfamily(b, l + 1);
    if (!r) r = typedeclnorm(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "family" kindedvars 'where' decls
  static boolean typedeclclosedfamily(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedeclclosedfamily")) return false;
    if (!nextTokenIs(b, FAMILYTOKEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, FAMILYTOKEN);
    r = r && kindedvars(b, l + 1);
    r = r && consumeToken(b, WHERE);
    p = r; // pin = 3
    r = r && decls(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // ["family" | "instance"] typee ['=' (typee|foralltype)]
  static boolean typedeclnorm(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedeclnorm")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typedeclnorm_0(b, l + 1);
    r = r && typee(b, l + 1);
    r = r && typedeclnorm_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ["family" | "instance"]
  private static boolean typedeclnorm_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedeclnorm_0")) return false;
    typedeclnorm_0_0(b, l + 1);
    return true;
  }

  // "family" | "instance"
  private static boolean typedeclnorm_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedeclnorm_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, FAMILYTOKEN);
    if (!r) r = consumeToken(b, INSTANCE);
    exit_section_(b, m, null, r);
    return r;
  }

  // ['=' (typee|foralltype)]
  private static boolean typedeclnorm_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedeclnorm_2")) return false;
    typedeclnorm_2_0(b, l + 1);
    return true;
  }

  // '=' (typee|foralltype)
  private static boolean typedeclnorm_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedeclnorm_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, EQUALS);
    r = r && typedeclnorm_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // typee|foralltype
  private static boolean typedeclnorm_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typedeclnorm_2_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typee(b, l + 1);
    if (!r) r = foralltype(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // btype [typeeaux]
  public static boolean typee(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typee")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<typee>");
    r = btype(b, l + 1);
    p = r; // pin = 1
    r = r && typee_1(b, l + 1);
    exit_section_(b, l, m, TYPEE, r, p, null);
    return r || p;
  }

  // [typeeaux]
  private static boolean typee_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typee_1")) return false;
    typeeaux(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // (singlequote (qconop | varop) | qtyconop) typee
  //                    | typeeopt
  static boolean typeeaux(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typeeaux")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typeeaux_0(b, l + 1);
    if (!r) r = typeeopt(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (singlequote (qconop | varop) | qtyconop) typee
  private static boolean typeeaux_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typeeaux_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typeeaux_0_0(b, l + 1);
    r = r && typee(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // singlequote (qconop | varop) | qtyconop
  private static boolean typeeaux_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typeeaux_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typeeaux_0_0_0(b, l + 1);
    if (!r) r = qtyconop(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // singlequote (qconop | varop)
  private static boolean typeeaux_0_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typeeaux_0_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SINGLEQUOTE);
    r = r && typeeaux_0_0_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // qconop | varop
  private static boolean typeeaux_0_0_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typeeaux_0_0_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qconop(b, l + 1);
    if (!r) r = varop(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "->" typee
  static boolean typeeopt(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typeeopt")) return false;
    if (!nextTokenIs(b, RIGHTARROW)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, RIGHTARROW);
    p = r; // pin = 1
    r = r && typee(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // varid
  public static boolean tyvar(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tyvar")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<tyvar>");
    r = varid(b, l + 1);
    exit_section_(b, l, m, TYVAR, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // varid | '(' varsym ')'
  static boolean var(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "var")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = varid(b, l + 1);
    if (!r) r = var_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' varsym ')'
  private static boolean var_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "var_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && varsym(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // varidRegexp | "as" | "rec" | "qualified"
  public static boolean varid(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "varid")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<varid>");
    r = consumeToken(b, VARIDREGEXP);
    if (!r) r = consumeToken(b, AS);
    if (!r) r = consumeToken(b, RECTOK);
    if (!r) r = consumeToken(b, QUALIFIED);
    exit_section_(b, l, m, VARID, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // varsym | '`' varid '`'
  public static boolean varop(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "varop")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<varop>");
    r = varsym(b, l + 1);
    if (!r) r = varop_1(b, l + 1);
    exit_section_(b, l, m, VAROP, r, false, null);
    return r;
  }

  // '`' varid '`'
  private static boolean varop_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "varop_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BACKTICK);
    r = r && varid(b, l + 1);
    r = r && consumeToken(b, BACKTICK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<sequence var>>
  public static boolean vars(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "vars")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<vars>");
    r = sequence(b, l + 1, var_parser_);
    exit_section_(b, l, m, VARS, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // VARSYMTOK | symbol1
  public static boolean varsym(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "varsym")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<varsym>");
    r = consumeToken(b, VARSYMTOK);
    if (!r) r = symbol1(b, l + 1);
    exit_section_(b, l, m, VARSYM, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // "where" decls
  static boolean wheredecls(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "wheredecls")) return false;
    if (!nextTokenIs(b, WHERE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, WHERE);
    p = r; // pin = 1
    r = r && decls(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  final static Parser cname_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return cname(b, l + 1);
    }
  };
  final static Parser con_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return con(b, l + 1);
    }
  };
  final static Parser ctype_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return ctype(b, l + 1);
    }
  };
  final static Parser dclass_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return dclass(b, l + 1);
    }
  };
  final static Parser export_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return export(b, l + 1);
    }
  };
  final static Parser fundep_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return fundep(b, l + 1);
    }
  };
  final static Parser importt_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return importt(b, l + 1);
    }
  };
  final static Parser op_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return op(b, l + 1);
    }
  };
  final static Parser qvar_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return qvar(b, l + 1);
    }
  };
  final static Parser typee_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return typee(b, l + 1);
    }
  };
  final static Parser var_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return var(b, l + 1);
    }
  };
}
