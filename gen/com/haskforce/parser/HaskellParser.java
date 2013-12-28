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
    if (root_ == ANY) {
      result_ = any(builder_, 0);
    }
    else if (root_ == ANY_1) {
      result_ = any1(builder_, 0);
    }
    else if (root_ == ASC_SYMBOL) {
      result_ = ascSymbol(builder_, 0);
    }
    else if (root_ == CHARTOKEN) {
      result_ = chartoken(builder_, 0);
    }
    else if (root_ == COMMENT) {
      result_ = comment(builder_, 0);
    }
    else if (root_ == CONID) {
      result_ = conid(builder_, 0);
    }
    else if (root_ == CONSYM) {
      result_ = consym(builder_, 0);
    }
    else if (root_ == DECIMAL) {
      result_ = decimal(builder_, 0);
    }
    else if (root_ == DIGIT) {
      result_ = digit(builder_, 0);
    }
    else if (root_ == ESCAPE) {
      result_ = escape(builder_, 0);
    }
    else if (root_ == EXPONENT) {
      result_ = exponent(builder_, 0);
    }
    else if (root_ == FLOATTOKEN) {
      result_ = floattoken(builder_, 0);
    }
    else if (root_ == GAP) {
      result_ = gap(builder_, 0);
    }
    else if (root_ == GRAPHIC) {
      result_ = graphic(builder_, 0);
    }
    else if (root_ == HEXADECIMAL) {
      result_ = hexadecimal(builder_, 0);
    }
    else if (root_ == INTEGERTOKEN) {
      result_ = integertoken(builder_, 0);
    }
    else if (root_ == LARGE) {
      result_ = large(builder_, 0);
    }
    else if (root_ == LEXEME) {
      result_ = lexeme(builder_, 0);
    }
    else if (root_ == LITERAL) {
      result_ = literal(builder_, 0);
    }
    else if (root_ == MODID) {
      result_ = modid(builder_, 0);
    }
    else if (root_ == NCOMMENT) {
      result_ = ncomment(builder_, 0);
    }
    else if (root_ == NEWLINE) {
      result_ = newline(builder_, 0);
    }
    else if (root_ == OCTAL) {
      result_ = octal(builder_, 0);
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
    else if (root_ == RESERVEDOP_WITH_CONS) {
      result_ = reservedopWithCons(builder_, 0);
    }
    else if (root_ == RESERVEDOP_WITHOUT_CONS) {
      result_ = reservedopWithoutCons(builder_, 0);
    }
    else if (root_ == SEQ) {
      result_ = seq(builder_, 0);
    }
    else if (root_ == SMALL) {
      result_ = small(builder_, 0);
    }
    else if (root_ == SPECIAL) {
      result_ = special(builder_, 0);
    }
    else if (root_ == STRINGTOKEN) {
      result_ = stringtoken(builder_, 0);
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
    else if (root_ == WHITESPACE) {
      result_ = whitespace(builder_, 0);
    }
    else if (root_ == WHITETOKEN) {
      result_ = whitetoken(builder_, 0);
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
  // graphic | whitechar
  public static boolean any(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "any")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<any>");
    result_ = graphic(builder_, level_ + 1);
    if (!result_) result_ = whitechar(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ANY, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // graphic | space | tab
  public static boolean any1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "any1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<any 1>");
    result_ = graphic(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, SPACE);
    if (!result_) result_ = consumeToken(builder_, TAB);
    exit_section_(builder_, level_, marker_, ANY_1, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
  //               | '\' | '^' | '|' | '-' | '~' | ':'
  public static boolean ascSymbol(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ascSymbol")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<asc symbol>");
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
    exit_section_(builder_, level_, marker_, ASC_SYMBOL, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // "'" (!("'" | '\') graphic | space | !'\&' escape) "'"
  public static boolean chartoken(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "chartoken")) return false;
    if (!nextTokenIs(builder_, SINGLEQUOTE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, SINGLEQUOTE);
    result_ = result_ && chartoken_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, SINGLEQUOTE);
    exit_section_(builder_, marker_, CHARTOKEN, result_);
    return result_;
  }

  // !("'" | '\') graphic | space | !'\&' escape
  private static boolean chartoken_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "chartoken_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = chartoken_1_0(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, SPACE);
    if (!result_) result_ = chartoken_1_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !("'" | '\') graphic
  private static boolean chartoken_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "chartoken_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = chartoken_1_0_0(builder_, level_ + 1);
    result_ = result_ && graphic(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !("'" | '\')
  private static boolean chartoken_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "chartoken_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !chartoken_1_0_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // "'" | '\'
  private static boolean chartoken_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "chartoken_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, SINGLEQUOTE);
    if (!result_) result_ = consumeToken(builder_, BACKSLASH);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !'\&' escape
  private static boolean chartoken_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "chartoken_1_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = chartoken_1_2_0(builder_, level_ + 1);
    result_ = result_ && escape(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !'\&'
  private static boolean chartoken_1_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "chartoken_1_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !consumeToken(builder_, NULLCHARACTER);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // dashes [ !symbol any1 {any1} * ] !newline
  public static boolean comment(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comment")) return false;
    if (!nextTokenIs(builder_, DASHES)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DASHES);
    result_ = result_ && comment_1(builder_, level_ + 1);
    result_ = result_ && comment_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, COMMENT, result_);
    return result_;
  }

  // [ !symbol any1 {any1} * ]
  private static boolean comment_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comment_1")) return false;
    comment_1_0(builder_, level_ + 1);
    return true;
  }

  // !symbol any1 {any1} *
  private static boolean comment_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comment_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = comment_1_0_0(builder_, level_ + 1);
    result_ = result_ && any1(builder_, level_ + 1);
    result_ = result_ && comment_1_0_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !symbol
  private static boolean comment_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comment_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !symbol(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // {any1} *
  private static boolean comment_1_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comment_1_0_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!comment_1_0_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "comment_1_0_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {any1}
  private static boolean comment_1_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comment_1_0_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = any1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !newline
  private static boolean comment_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comment_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !newline(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // large {small | large | digit | "'" } *
  public static boolean conid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "conid")) return false;
    if (!nextTokenIs(builder_, ASCLARGE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = large(builder_, level_ + 1);
    result_ = result_ && conid_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, CONID, result_);
    return result_;
  }

  // {small | large | digit | "'" } *
  private static boolean conid_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "conid_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!conid_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "conid_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // small | large | digit | "'"
  private static boolean conid_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "conid_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = small(builder_, level_ + 1);
    if (!result_) result_ = large(builder_, level_ + 1);
    if (!result_) result_ = digit(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, SINGLEQUOTE);
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
  // digit {digit} *
  public static boolean decimal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decimal")) return false;
    if (!nextTokenIs(builder_, ASCDIGIT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = digit(builder_, level_ + 1);
    result_ = result_ && decimal_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, DECIMAL, result_);
    return result_;
  }

  // {digit} *
  private static boolean decimal_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decimal_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!decimal_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "decimal_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {digit}
  private static boolean decimal_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "decimal_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = digit(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ascDigit
  public static boolean digit(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "digit")) return false;
    if (!nextTokenIs(builder_, ASCDIGIT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ASCDIGIT);
    exit_section_(builder_, marker_, DIGIT, result_);
    return result_;
  }

  /* ********************************************************** */
  // '\' ( charesc | ascii | decimal | 'o' octal | 'x' hexadecimal )
  public static boolean escape(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "escape")) return false;
    if (!nextTokenIs(builder_, BACKSLASH)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKSLASH);
    result_ = result_ && escape_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, ESCAPE, result_);
    return result_;
  }

  // charesc | ascii | decimal | 'o' octal | 'x' hexadecimal
  private static boolean escape_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "escape_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, CHARESC);
    if (!result_) result_ = consumeToken(builder_, ASCII);
    if (!result_) result_ = decimal(builder_, level_ + 1);
    if (!result_) result_ = escape_1_3(builder_, level_ + 1);
    if (!result_) result_ = escape_1_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // 'o' octal
  private static boolean escape_1_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "escape_1_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "o");
    result_ = result_ && octal(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // 'x' hexadecimal
  private static boolean escape_1_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "escape_1_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "x");
    result_ = result_ && hexadecimal(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ('e' | 'E') ['+' | '-'] decimal
  public static boolean exponent(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exponent")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<exponent>");
    result_ = exponent_0(builder_, level_ + 1);
    result_ = result_ && exponent_1(builder_, level_ + 1);
    result_ = result_ && decimal(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, EXPONENT, result_, false, null);
    return result_;
  }

  // 'e' | 'E'
  private static boolean exponent_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exponent_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "e");
    if (!result_) result_ = consumeToken(builder_, "E");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ['+' | '-']
  private static boolean exponent_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exponent_1")) return false;
    exponent_1_0(builder_, level_ + 1);
    return true;
  }

  // '+' | '-'
  private static boolean exponent_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exponent_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, PLUS);
    if (!result_) result_ = consumeToken(builder_, MINUS);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // decimal '.' decimal [exponent]
  //               | decimal exponent
  public static boolean floattoken(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "floattoken")) return false;
    if (!nextTokenIs(builder_, ASCDIGIT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = floattoken_0(builder_, level_ + 1);
    if (!result_) result_ = floattoken_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, FLOATTOKEN, result_);
    return result_;
  }

  // decimal '.' decimal [exponent]
  private static boolean floattoken_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "floattoken_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = decimal(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PERIOD);
    result_ = result_ && decimal(builder_, level_ + 1);
    result_ = result_ && floattoken_0_3(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [exponent]
  private static boolean floattoken_0_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "floattoken_0_3")) return false;
    exponent(builder_, level_ + 1);
    return true;
  }

  // decimal exponent
  private static boolean floattoken_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "floattoken_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = decimal(builder_, level_ + 1);
    result_ = result_ && exponent(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '\' whitechar {whitechar} * '\'
  public static boolean gap(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gap")) return false;
    if (!nextTokenIs(builder_, BACKSLASH)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, BACKSLASH);
    result_ = result_ && whitechar(builder_, level_ + 1);
    result_ = result_ && gap_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, BACKSLASH);
    exit_section_(builder_, marker_, GAP, result_);
    return result_;
  }

  // {whitechar} *
  private static boolean gap_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gap_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!gap_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "gap_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {whitechar}
  private static boolean gap_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "gap_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = whitechar(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // small | large | symbol | digit | special | '"' | "'"
  public static boolean graphic(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "graphic")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<graphic>");
    result_ = small(builder_, level_ + 1);
    if (!result_) result_ = large(builder_, level_ + 1);
    if (!result_) result_ = symbol(builder_, level_ + 1);
    if (!result_) result_ = digit(builder_, level_ + 1);
    if (!result_) result_ = special(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, DOUBLEQUOTE);
    if (!result_) result_ = consumeToken(builder_, SINGLEQUOTE);
    exit_section_(builder_, level_, marker_, GRAPHIC, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // hexit {hexit} *
  public static boolean hexadecimal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "hexadecimal")) return false;
    if (!nextTokenIs(builder_, HEXIT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, HEXIT);
    result_ = result_ && hexadecimal_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, HEXADECIMAL, result_);
    return result_;
  }

  // {hexit} *
  private static boolean hexadecimal_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "hexadecimal_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consumeToken(builder_, HEXIT)) break;
      if (!empty_element_parsed_guard_(builder_, "hexadecimal_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // decimal
  //                | octalPrefix octal
  //                | hexadecimalPrefix hexadecimal
  public static boolean integertoken(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "integertoken")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<integertoken>");
    result_ = decimal(builder_, level_ + 1);
    if (!result_) result_ = integertoken_1(builder_, level_ + 1);
    if (!result_) result_ = integertoken_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, INTEGERTOKEN, result_, false, null);
    return result_;
  }

  // octalPrefix octal
  private static boolean integertoken_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "integertoken_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OCTALPREFIX);
    result_ = result_ && octal(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // hexadecimalPrefix hexadecimal
  private static boolean integertoken_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "integertoken_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, HEXADECIMALPREFIX);
    result_ = result_ && hexadecimal(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ascLarge
  public static boolean large(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "large")) return false;
    if (!nextTokenIs(builder_, ASCLARGE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ASCLARGE);
    exit_section_(builder_, marker_, LARGE, result_);
    return result_;
  }

  /* ********************************************************** */
  // qvarid | qconid | qvarsym | qconsym
  //               |	literal | special | reservedop | reservedid
  public static boolean lexeme(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lexeme")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<lexeme>");
    result_ = qvarid(builder_, level_ + 1);
    if (!result_) result_ = qconid(builder_, level_ + 1);
    if (!result_) result_ = qvarsym(builder_, level_ + 1);
    if (!result_) result_ = qconsym(builder_, level_ + 1);
    if (!result_) result_ = literal(builder_, level_ + 1);
    if (!result_) result_ = special(builder_, level_ + 1);
    if (!result_) result_ = reservedop(builder_, level_ + 1);
    if (!result_) result_ = reservedid(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, LEXEME, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // integertoken | floattoken | chartoken | stringtoken
  public static boolean literal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "literal")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<literal>");
    result_ = integertoken(builder_, level_ + 1);
    if (!result_) result_ = floattoken(builder_, level_ + 1);
    if (!result_) result_ = chartoken(builder_, level_ + 1);
    if (!result_) result_ = stringtoken(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, LITERAL, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // {conid '.'} * conid
  public static boolean modid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "modid")) return false;
    if (!nextTokenIs(builder_, ASCLARGE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = modid_0(builder_, level_ + 1);
    result_ = result_ && conid(builder_, level_ + 1);
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
    result_ = conid(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, PERIOD);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // opencom any seq {ncomment any seq} * closecom
  // any
  public static boolean ncomment(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment")) return false;
    if (!nextTokenIs(builder_, OPENCOM)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OPENCOM);
    result_ = result_ && any(builder_, level_ + 1);
    result_ = result_ && seq(builder_, level_ + 1);
    result_ = result_ && ncomment_3(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, CLOSECOM);
    result_ = result_ && any(builder_, level_ + 1);
    exit_section_(builder_, marker_, NCOMMENT, result_);
    return result_;
  }

  // {ncomment any seq} *
  private static boolean ncomment_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_3")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!ncomment_3_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "ncomment_3", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // ncomment any seq
  private static boolean ncomment_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "ncomment_3_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = ncomment(builder_, level_ + 1);
    result_ = result_ && any(builder_, level_ + 1);
    result_ = result_ && seq(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // return linefeed | return | linefeed | formfeed
  public static boolean newline(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "newline")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<newline>");
    result_ = parseTokens(builder_, 0, RETURN, LINEFEED);
    if (!result_) result_ = consumeToken(builder_, RETURN);
    if (!result_) result_ = consumeToken(builder_, LINEFEED);
    if (!result_) result_ = consumeToken(builder_, FORMFEED);
    exit_section_(builder_, level_, marker_, NEWLINE, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // octit {octit} *
  public static boolean octal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "octal")) return false;
    if (!nextTokenIs(builder_, OCTIT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OCTIT);
    result_ = result_ && octal_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, OCTAL, result_);
    return result_;
  }

  // {octit} *
  private static boolean octal_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "octal_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!consumeToken(builder_, OCTIT)) break;
      if (!empty_element_parsed_guard_(builder_, "octal_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  /* ********************************************************** */
  // { lexeme | whitespace } *
  static boolean program(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "program")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!program_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "program", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // lexeme | whitespace
  private static boolean program_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "program_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = lexeme(builder_, level_ + 1);
    if (!result_) result_ = whitespace(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // [modid '.'] conid
  public static boolean qconid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qconid")) return false;
    if (!nextTokenIs(builder_, ASCLARGE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qconid_0(builder_, level_ + 1);
    result_ = result_ && conid(builder_, level_ + 1);
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
    if (!nextTokenIs(builder_, "<qconsym>", COLON, ASCLARGE)) return false;
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
    if (!nextTokenIs(builder_, "<qvarid>", ASCLARGE, ASCSMALL)) return false;
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
  // case | classtoken | data | default | deriving | do | else
  //               | foreign | if | import | in | infix | infixl
  //               | infixr | instance | let | module | newtype | of
  //               | then | type | where | '_'
  public static boolean reservedid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedid")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<reservedid>");
    result_ = consumeToken(builder_, CASE);
    if (!result_) result_ = consumeToken(builder_, CLASSTOKEN);
    if (!result_) result_ = consumeToken(builder_, DATA);
    if (!result_) result_ = consumeToken(builder_, DEFAULT);
    if (!result_) result_ = consumeToken(builder_, DERIVING);
    if (!result_) result_ = consumeToken(builder_, DO);
    if (!result_) result_ = consumeToken(builder_, ELSE);
    if (!result_) result_ = consumeToken(builder_, FOREIGN);
    if (!result_) result_ = consumeToken(builder_, IF);
    if (!result_) result_ = consumeToken(builder_, IMPORT);
    if (!result_) result_ = consumeToken(builder_, IN);
    if (!result_) result_ = consumeToken(builder_, INFIX);
    if (!result_) result_ = consumeToken(builder_, INFIXL);
    if (!result_) result_ = consumeToken(builder_, INFIXR);
    if (!result_) result_ = consumeToken(builder_, INSTANCE);
    if (!result_) result_ = consumeToken(builder_, LET);
    if (!result_) result_ = consumeToken(builder_, MODULE);
    if (!result_) result_ = consumeToken(builder_, NEWTYPE);
    if (!result_) result_ = consumeToken(builder_, OF);
    if (!result_) result_ = consumeToken(builder_, THEN);
    if (!result_) result_ = consumeToken(builder_, TYPE);
    if (!result_) result_ = consumeToken(builder_, WHERE);
    if (!result_) result_ = consumeToken(builder_, UNDERSCORE);
    exit_section_(builder_, level_, marker_, RESERVEDID, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // reservedopWithCons
  public static boolean reservedop(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedop")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<reservedop>");
    result_ = reservedopWithCons(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, RESERVEDOP, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // ':' | reservedopWithoutCons
  public static boolean reservedopWithCons(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "reservedopWithCons")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<reservedop with cons>");
    result_ = consumeToken(builder_, COLON);
    if (!result_) result_ = reservedopWithoutCons(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, RESERVEDOP_WITH_CONS, result_, false, null);
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
  // !({any} * ( opencom | closecom ) {any} *) {any} *
  public static boolean seq(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<seq>");
    result_ = seq_0(builder_, level_ + 1);
    result_ = result_ && seq_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, SEQ, result_, false, null);
    return result_;
  }

  // !({any} * ( opencom | closecom ) {any} *)
  private static boolean seq_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !seq_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // {any} * ( opencom | closecom ) {any} *
  private static boolean seq_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = seq_0_0_0(builder_, level_ + 1);
    result_ = result_ && seq_0_0_1(builder_, level_ + 1);
    result_ = result_ && seq_0_0_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // {any} *
  private static boolean seq_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_0_0_0")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!seq_0_0_0_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "seq_0_0_0", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {any}
  private static boolean seq_0_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_0_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = any(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // opencom | closecom
  private static boolean seq_0_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_0_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, OPENCOM);
    if (!result_) result_ = consumeToken(builder_, CLOSECOM);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // {any} *
  private static boolean seq_0_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_0_0_2")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!seq_0_0_2_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "seq_0_0_2", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {any}
  private static boolean seq_0_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_0_0_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = any(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // {any} *
  private static boolean seq_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!seq_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "seq_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // {any}
  private static boolean seq_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "seq_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = any(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ascSmall | '_'
  public static boolean small(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "small")) return false;
    if (!nextTokenIs(builder_, "<small>", UNDERSCORE, ASCSMALL)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<small>");
    result_ = consumeToken(builder_, ASCSMALL);
    if (!result_) result_ = consumeToken(builder_, UNDERSCORE);
    exit_section_(builder_, level_, marker_, SMALL, result_, false, null);
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
  // '"' { !('"' | '\') graphic | space | escape | gap} * '"'
  public static boolean stringtoken(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stringtoken")) return false;
    if (!nextTokenIs(builder_, DOUBLEQUOTE)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOUBLEQUOTE);
    result_ = result_ && stringtoken_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, DOUBLEQUOTE);
    exit_section_(builder_, marker_, STRINGTOKEN, result_);
    return result_;
  }

  // { !('"' | '\') graphic | space | escape | gap} *
  private static boolean stringtoken_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stringtoken_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!stringtoken_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "stringtoken_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // !('"' | '\') graphic | space | escape | gap
  private static boolean stringtoken_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stringtoken_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = stringtoken_1_0_0(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, SPACE);
    if (!result_) result_ = escape(builder_, level_ + 1);
    if (!result_) result_ = gap(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !('"' | '\') graphic
  private static boolean stringtoken_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stringtoken_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = stringtoken_1_0_0_0(builder_, level_ + 1);
    result_ = result_ && graphic(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !('"' | '\')
  private static boolean stringtoken_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stringtoken_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !stringtoken_1_0_0_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // '"' | '\'
  private static boolean stringtoken_1_0_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "stringtoken_1_0_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, DOUBLEQUOTE);
    if (!result_) result_ = consumeToken(builder_, BACKSLASH);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ascSymbol
  public static boolean symbol(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "symbol")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<symbol>");
    result_ = ascSymbol(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, SYMBOL, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // !reservedid (small {small | large | digit | "'" } *)
  public static boolean varid(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid")) return false;
    if (!nextTokenIs(builder_, ASCSMALL)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = varid_0(builder_, level_ + 1);
    result_ = result_ && varid_1(builder_, level_ + 1);
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

  // small {small | large | digit | "'" } *
  private static boolean varid_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = small(builder_, level_ + 1);
    result_ = result_ && varid_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // {small | large | digit | "'" } *
  private static boolean varid_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid_1_1")) return false;
    int pos_ = current_position_(builder_);
    while (true) {
      if (!varid_1_1_0(builder_, level_ + 1)) break;
      if (!empty_element_parsed_guard_(builder_, "varid_1_1", pos_)) break;
      pos_ = current_position_(builder_);
    }
    return true;
  }

  // small | large | digit | "'"
  private static boolean varid_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varid_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = small(builder_, level_ + 1);
    if (!result_) result_ = large(builder_, level_ + 1);
    if (!result_) result_ = digit(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, SINGLEQUOTE);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !(reservedop | dashes) ( !':' symbol {symbol} * )
  public static boolean varsym(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "varsym")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<varsym>");
    result_ = varsym_0(builder_, level_ + 1);
    result_ = result_ && varsym_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, VARSYM, result_, false, null);
    return result_;
  }

  // !(reservedop | dashes)
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
  // newline | vertab | space | tab
  public static boolean whitechar(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitechar")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<whitechar>");
    result_ = newline(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, VERTAB);
    if (!result_) result_ = consumeToken(builder_, SPACE);
    if (!result_) result_ = consumeToken(builder_, TAB);
    exit_section_(builder_, level_, marker_, WHITECHAR, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // whitetoken {whitetoken} *
  public static boolean whitespace(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitespace")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<whitespace>");
    result_ = whitetoken(builder_, level_ + 1);
    result_ = result_ && whitespace_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, WHITESPACE, result_, false, null);
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
  // whitechar | comment | ncomment
  public static boolean whitetoken(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "whitetoken")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<whitetoken>");
    result_ = whitechar(builder_, level_ + 1);
    if (!result_) result_ = comment(builder_, level_ + 1);
    if (!result_) result_ = ncomment(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, WHITETOKEN, result_, false, null);
    return result_;
  }

}
