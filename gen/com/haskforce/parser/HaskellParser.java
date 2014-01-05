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
    else if (root_ == CONSYM) {
      result_ = consym(builder_, 0);
    }
    else if (root_ == MODID) {
      result_ = modid(builder_, 0);
    }
    else if (root_ == NCOMMENT) {
      result_ = ncomment(builder_, 0);
    }
    else if (root_ == QCONID) {
      result_ = qconid(builder_, 0);
    }
    else if (root_ == QCONSYM) {
      result_ = qconsym(builder_, 0);
    }
    else if (root_ == QVARID) {
      result_ = qvarid(builder_, 0);
    }
    else if (root_ == QVARSYM) {
      result_ = qvarsym(builder_, 0);
    }
    else if (root_ == RESERVEDID) {
      result_ = reservedid(builder_, 0);
    }
    else if (root_ == RESERVEDOP) {
      result_ = reservedop(builder_, 0);
    }
    else if (root_ == RESERVEDOP_WITHOUT_CONS) {
      result_ = reservedopWithoutCons(builder_, 0);
    }
    else if (root_ == SPECIAL) {
      result_ = special(builder_, 0);
    }
    else if (root_ == SYMBOL) {
      result_ = symbol(builder_, 0);
    }
    else if (root_ == VARID) {
      result_ = varid(builder_, 0);
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
    return program(builder_, level_ + 1);
  }

  /* ********************************************************** */
  // !( opencom | closecom ) (lexeme | space) *
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

  // (lexeme | space) *
  private static boolean anyseq_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anyseq_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!anyseq_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "anyseq_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // lexeme | space
  private static boolean anyseq_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anyseq_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = lexeme(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, SPACE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !reservedopWithoutCons ( ':' {symbol} *)
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

  // !reservedopWithoutCons
  private static boolean consym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !reservedopWithoutCons(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // ':' {symbol} *
  private static boolean consym_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, COLON);
    result_ = result_ && consym_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // {symbol} *
  private static boolean consym_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym_1_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consym_1_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "consym_1_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {symbol}
  private static boolean consym_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "consym_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = symbol(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // qvarid | qconid | qvarsym | qconsym
  //                  | literal | special | reservedop | reservedid
  static boolean lexeme(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexeme")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qvarid(builder_, level_ + 1);
    if (!result_) result_ = qconid(builder_, level_ + 1);
    if (!result_) result_ = qvarsym(builder_, level_ + 1);
    if (!result_) result_ = qconsym(builder_, level_ + 1);
    if (!result_) result_ = literal(builder_, level_ + 1);
    if (!result_) result_ = special(builder_, level_ + 1);
    if (!result_) result_ = reservedop(builder_, level_ + 1);
    if (!result_) result_ = reservedid(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // integertoken | floattoken | chartoken | stringtoken
  static boolean literal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, INTEGERTOKEN);
    if (!result_) result_ = consumeToken(builder_, FLOATTOKEN);
    if (!result_) result_ = consumeToken(builder_, CHARTOKEN);
    if (!result_) result_ = consumeToken(builder_, STRINGTOKEN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // {conid '.'} * conid
  public static boolean modid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modid")) return false;
    if (!nextTokenIs(builder_, CONID)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = modid_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CONID);
    exit_section_(builder_, marker_, MODID, result_);
    return result_;
  }

  // {conid '.'} *
  private static boolean modid_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modid_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!modid_0_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "modid_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // conid '.'
  private static boolean modid_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modid_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CONID);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // opencom anyseq {ncomment anyseq} * closecom
  public static boolean ncomment(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment")) return false;
    if (!nextTokenIs(builder_, OPENCOM)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OPENCOM);
    result_ = result_ && anyseq(builder_, level_ + 1);
    result_ = result_ && ncomment_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CLOSECOM);
    exit_section_(builder_, marker_, NCOMMENT, result_);
    return result_;
  }

  // {ncomment anyseq} *
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

  // ncomment anyseq
  private static boolean ncomment_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ncomment(builder_, level_ + 1);
    result_ = result_ && anyseq(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // {pragma} * { whitespace | lexeme } *
  static boolean program(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "program")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = program_0(builder_, level_ + 1);
    result_ = result_ && program_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // {pragma} *
  private static boolean program_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "program_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consumeToken(builder_, PRAGMA)) break;
      if (!empty_element_parsed_guard_(builder_, "program_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // { whitespace | lexeme } *
  private static boolean program_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "program_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!program_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "program_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // whitespace | lexeme
  private static boolean program_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "program_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitespace(builder_, level_ + 1);
    if (!result_) result_ = lexeme(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modid '.'] conid
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

  // [modid '.']
  private static boolean qconid_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconid_0")) return false;
    qconid_0_0(builder_, level_ + 1);
    return true;
  }

  // modid '.'
  private static boolean qconid_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconid_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = modid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modid '.'] consym
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

  // [modid '.']
  private static boolean qconsym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconsym_0")) return false;
    qconsym_0_0(builder_, level_ + 1);
    return true;
  }

  // modid '.'
  private static boolean qconsym_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconsym_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = modid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modid '.'] varid
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

  // [modid '.']
  private static boolean qvarid_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarid_0")) return false;
    qvarid_0_0(builder_, level_ + 1);
    return true;
  }

  // modid '.'
  private static boolean qvarid_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarid_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = modid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modid '.'] varsym
  public static boolean qvarsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<qvarsym>");
    result_ = qvarsym_0(builder_, level_ + 1);
    result_ = result_ && varsym(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, QVARSYM, result_, false, null);
    return result_;
  }

  // [modid '.']
  private static boolean qvarsym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarsym_0")) return false;
    qvarsym_0_0(builder_, level_ + 1);
    return true;
  }

  // modid '.'
  private static boolean qvarsym_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qvarsym_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = modid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // 'case' | 'class' | 'data' | 'default' | 'deriving' | 'do' | 'else'
  //               | 'foreign' | 'if' | 'import' | 'infix' | 'infixl' | 'infixr'
  //               | 'instance' | 'in' | 'let' | 'module' | 'newtype' | 'of' | 'then' | 'type'
  //               | 'where' | '_'
  public static boolean reservedid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedid")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<reservedid>");
    result_ = consumeToken(builder_, "case");
    if (!result_) result_ = consumeToken(builder_, CLASSTOKEN);
    if (!result_) result_ = consumeToken(builder_, "data");
    if (!result_) result_ = consumeToken(builder_, "default");
    if (!result_) result_ = consumeToken(builder_, "deriving");
    if (!result_) result_ = consumeToken(builder_, "do");
    if (!result_) result_ = consumeToken(builder_, "else");
    if (!result_) result_ = consumeToken(builder_, "foreign");
    if (!result_) result_ = consumeToken(builder_, "if");
    if (!result_) result_ = consumeToken(builder_, "import");
    if (!result_) result_ = consumeToken(builder_, "infix");
    if (!result_) result_ = consumeToken(builder_, "infixl");
    if (!result_) result_ = consumeToken(builder_, "infixr");
    if (!result_) result_ = consumeToken(builder_, "instance");
    if (!result_) result_ = consumeToken(builder_, "in");
    if (!result_) result_ = consumeToken(builder_, "let");
    if (!result_) result_ = consumeToken(builder_, "module");
    if (!result_) result_ = consumeToken(builder_, "newtype");
    if (!result_) result_ = consumeToken(builder_, "of");
    if (!result_) result_ = consumeToken(builder_, "then");
    if (!result_) result_ = consumeToken(builder_, "type");
    if (!result_) result_ = consumeToken(builder_, "where");
    if (!result_) result_ = consumeToken(builder_, "_");
    exit_section_(builder_, level_, marker_, RESERVEDID, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // ':' | reservedopWithoutCons
  public static boolean reservedop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<reservedop>");
    result_ = consumeToken(builder_, COLON);
    if (!result_) result_ = reservedopWithoutCons(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, RESERVEDOP, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '..' | '::' | '=' | '\' | '|' | '<-' | '->' | '@' | '~' | '=>'
  public static boolean reservedopWithoutCons(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedopWithoutCons")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<reservedop without cons>");
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
    exit_section_(builder_, level_, marker_, RESERVEDOP_WITHOUT_CONS, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}'
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
    if (!result_) result_ = consumeToken(builder_, BACKTICK);
    if (!result_) result_ = consumeToken(builder_, LBRACE);
    if (!result_) result_ = consumeToken(builder_, RBRACE);
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
  // !(reservedid | dashes) varidRegexp
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

  // !(reservedid | dashes)
  private static boolean varid_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !varid_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // reservedid | dashes
  private static boolean varid_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = reservedid(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, DASHES);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !(reservedop whitetoken) ( !':' symbol {symbol} * )
  public static boolean varsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<varsym>");
    result_ = varsym_0(builder_, level_ + 1);
    result_ = result_ && varsym_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, VARSYM, result_, false, null);
    return result_;
  }

  // !(reservedop whitetoken)
  private static boolean varsym_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !varsym_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // reservedop whitetoken
  private static boolean varsym_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = reservedop(builder_, level_ + 1);
    result_ = result_ && whitetoken(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !':' symbol {symbol} *
  private static boolean varsym_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = varsym_1_0(builder_, level_ + 1);
    result_ = result_ && symbol(builder_, level_ + 1);
    result_ = result_ && varsym_1_2(builder_, level_ + 1);
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

  // {symbol} *
  private static boolean varsym_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_1_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!varsym_1_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "varsym_1_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {symbol}
  private static boolean varsym_1_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym_1_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = symbol(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // space | whiteEscapes
  public static boolean whitechar(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitechar")) return false;
    if (!nextTokenIs(builder_, "<whitechar>", SPACE, WHITEESCAPES)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<whitechar>");
    result_ = consumeToken(builder_, SPACE);
    if (!result_) result_ = consumeToken(builder_, WHITEESCAPES);
    exit_section_(builder_, level_, marker_, WHITECHAR, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // whitetoken {whitetoken} *
  static boolean whitespace(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitespace")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitetoken(builder_, level_ + 1);
    result_ = result_ && whitespace_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // {whitetoken} *
  private static boolean whitespace_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitespace_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!whitespace_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "whitespace_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {whitetoken}
  private static boolean whitespace_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitespace_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitetoken(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // space | whitechar | comment | ncomment | haddock
  static boolean whitetoken(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitetoken")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, SPACE);
    if (!result_) result_ = whitechar(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, COMMENT);
    if (!result_) result_ = ncomment(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, HADDOCK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

}
