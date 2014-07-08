// This is a generated file. Not intended for manual editing.
package com.haskforce.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.openapi.diagnostic.Logger;
import static com.haskforce.psi.HaskellTypes.*;
import static com.intellij.lang.parser.GeneratedParserUtilBase.*;
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
    if (root_ == BODY) {
      result_ = body(builder_, 0);
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
    else if (root_ == CONSYM) {
      result_ = consym(builder_, 0);
    }
    else if (root_ == CPP) {
      result_ = cpp(builder_, 0);
    }
    else if (root_ == EXPORT) {
      result_ = export(builder_, 0);
    }
    else if (root_ == EXPORTS) {
      result_ = exports(builder_, 0);
    }
    else if (root_ == IMPDECL) {
      result_ = impdecl(builder_, 0);
    }
    else if (root_ == IMPORTT) {
      result_ = importt(builder_, 0);
    }
    else if (root_ == MODULE_PREFIX) {
      result_ = modulePrefix(builder_, 0);
    }
    else if (root_ == NCOMMENT) {
      result_ = ncomment(builder_, 0);
    }
    else if (root_ == PPRAGMA) {
      result_ = ppragma(builder_, 0);
    }
    else if (root_ == PSTRINGTOKEN) {
      result_ = pstringtoken(builder_, 0);
    }
    else if (root_ == QCONID) {
      result_ = qconid(builder_, 0);
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
    else if (root_ == QVARS) {
      result_ = qvars(builder_, 0);
    }
    else if (root_ == QVARSYM) {
      result_ = qvarsym(builder_, 0);
    }
    else if (root_ == RESERVEDOP) {
      result_ = reservedop(builder_, 0);
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
    else if (root_ == TYVAR) {
      result_ = tyvar(builder_, 0);
    }
    else if (root_ == VARID) {
      result_ = varid(builder_, 0);
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
  // cpp
  //         | qvar
  //         | qtycon ["(..)" | cnames]
  //         | qtycls ["(..)" | qvars]
  //         | "module" qconid
  public static boolean export(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<export>");
    result_ = cpp(builder_, level_ + 1);
    if (!result_) result_ = qvar(builder_, level_ + 1);
    if (!result_) result_ = export_2(builder_, level_ + 1);
    if (!result_) result_ = export_3(builder_, level_ + 1);
    if (!result_) result_ = export_4(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, EXPORT, result_, false, null);
    return result_;
  }

  // qtycon ["(..)" | cnames]
  private static boolean export_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qtycon(builder_, level_ + 1);
    result_ = result_ && export_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
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
    Marker marker_ = enter_section_(builder_);
    result_ = qtycls(builder_, level_ + 1);
    result_ = result_ && export_3_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
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
  // cpp
  //           | "import" ["qualified"] qconid ["as" qconid] [impspec]
  public static boolean impdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<impdecl>");
    result_ = cpp(builder_, level_ + 1);
    if (!result_) result_ = impdecl_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, IMPDECL, result_, false, null);
    return result_;
  }

  // "import" ["qualified"] qconid ["as" qconid] [impspec]
  private static boolean impdecl_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, IMPORT);
    result_ = result_ && impdecl_1_1(builder_, level_ + 1);
    result_ = result_ && qconid(builder_, level_ + 1);
    result_ = result_ && impdecl_1_3(builder_, level_ + 1);
    result_ = result_ && impdecl_1_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ["qualified"]
  private static boolean impdecl_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_1_1")) return false;
    consumeToken(builder_, QUALIFIED);
    return true;
  }

  // ["as" qconid]
  private static boolean impdecl_1_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_1_3")) return false;
    impdecl_1_3_0(builder_, level_ + 1);
    return true;
  }

  // "as" qconid
  private static boolean impdecl_1_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_1_3_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, AS);
    result_ = result_ && qconid(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [impspec]
  private static boolean impdecl_1_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "impdecl_1_4")) return false;
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
  // cpp | literal | reservedop | reservedid | ppragma
  //                  | qconid | qinfixvarid | qvarid | qinfixconid | qvarsym
  //                  | qconsym | special | symbol | "'" | "''"
  static boolean lexeme(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexeme")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = cpp(builder_, level_ + 1);
    if (!result_) result_ = literal(builder_, level_ + 1);
    if (!result_) result_ = reservedop(builder_, level_ + 1);
    if (!result_) result_ = reservedid(builder_, level_ + 1);
    if (!result_) result_ = ppragma(builder_, level_ + 1);
    if (!result_) result_ = qconid(builder_, level_ + 1);
    if (!result_) result_ = qinfixvarid(builder_, level_ + 1);
    if (!result_) result_ = qvarid(builder_, level_ + 1);
    if (!result_) result_ = qinfixconid(builder_, level_ + 1);
    if (!result_) result_ = qvarsym(builder_, level_ + 1);
    if (!result_) result_ = qconsym(builder_, level_ + 1);
    if (!result_) result_ = special(builder_, level_ + 1);
    if (!result_) result_ = symbol(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, SINGLEQUOTE);
    if (!result_) result_ = consumeToken(builder_, THQUOTE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // floattoken | integertoken | chartoken | pstringtoken
  static boolean literal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, FLOATTOKEN);
    if (!result_) result_ = consumeToken(builder_, INTEGERTOKEN);
    if (!result_) result_ = consumeToken(builder_, CHARTOKEN);
    if (!result_) result_ = pstringtoken(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // (whitespace | ppragma | cpp) module
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

  // (whitespace | ppragma | cpp) module
  private static boolean module_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_0_0(builder_, level_ + 1);
    result_ = result_ && module(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // whitespace | ppragma | cpp
  private static boolean module_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitespace(builder_, level_ + 1);
    if (!result_) result_ = ppragma(builder_, level_ + 1);
    if (!result_) result_ = cpp(builder_, level_ + 1);
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
  // opencom commenttext* (ncomment commenttext*)* closecom
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

  // commenttext*
  private static boolean ncomment_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consumeToken(builder_, COMMENTTEXT)) break;
      if (!empty_element_parsed_guard_(builder_, "ncomment_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // (ncomment commenttext*)*
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

  // ncomment commenttext*
  private static boolean ncomment_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ncomment(builder_, level_ + 1);
    result_ = result_ && ncomment_2_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // commenttext*
  private static boolean ncomment_2_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_2_0_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consumeToken(builder_, COMMENTTEXT)) break;
      if (!empty_element_parsed_guard_(builder_, "ncomment_2_0_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // EOL
  static boolean newline(PsiBuilder builder_, int level_) {
    return consumeToken(builder_, EOL);
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
    if (!nextTokenIs(builder_, "<qvarid>", CONIDREGEXP, VARIDREGEXP)) return false;
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
  // 'as' | 'export' | 'foreign' | 'import' | 'infix'
  //                        | 'infixl' | 'infixr'
  static boolean reservedMeta(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedMeta")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, AS);
    if (!result_) result_ = consumeToken(builder_, EXPORTTOKEN);
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
  // whitespace | lexeme
  static boolean topdecl(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "topdecl")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitespace(builder_, level_ + 1);
    if (!result_) result_ = lexeme(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // conidRegexp
  public static boolean tycls(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tycls")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONIDREGEXP);
    exit_section_(builder_, marker_, TYCLS, result_);
    return result_;
  }

  /* ********************************************************** */
  // conidRegexp
  public static boolean tycon(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tycon")) return false;
    if (!nextTokenIs(builder_, CONIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONIDREGEXP);
    exit_section_(builder_, marker_, TYCON, result_);
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
  static boolean var(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "var")) return false;
    if (!nextTokenIs(builder_, "", LPAREN, VARIDREGEXP)) return false;
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
  // !reservedid varidRegexp
  public static boolean varid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid")) return false;
    if (!nextTokenIs(builder_, VARIDREGEXP)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = varid_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, VARIDREGEXP);
    exit_section_(builder_, marker_, VARID, result_);
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

  final static Parser cname_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return cname(builder_, level_ + 1);
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
  final static Parser qvar_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return qvar(builder_, level_ + 1);
    }
  };
  final static Parser var_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return var(builder_, level_ + 1);
    }
  };
}
