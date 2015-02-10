// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static com.haskforce.cabal.psi.CabalTypes.*;
import static com.intellij.lang.parser.GeneratedParserUtilBase.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class CabalParser implements PsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, null);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    if (t == COMPLEXKEY) {
      r = complexkey(b, 0);
    }
    else if (t == CONDITIONAL) {
      r = conditional(b, 0);
    }
    else if (t == CONFIG) {
      r = config(b, 0);
    }
    else if (t == DEPENDENCY) {
      r = dependency(b, 0);
    }
    else if (t == DEPENDENCY_NAME) {
      r = dependencyName(b, 0);
    }
    else if (t == EXECUTABLE) {
      r = executable(b, 0);
    }
    else if (t == FLAG) {
      r = flag(b, 0);
    }
    else if (t == KEY) {
      r = key(b, 0);
    }
    else if (t == KEY_OR_CONFIG) {
      r = keyOrConfig(b, 0);
    }
    else if (t == LIBRARY) {
      r = library(b, 0);
    }
    else if (t == MODULE) {
      r = module(b, 0);
    }
    else if (t == NUMBER) {
      r = number(b, 0);
    }
    else if (t == SIMPLEKEY) {
      r = simplekey(b, 0);
    }
    else if (t == VARID) {
      r = varid(b, 0);
    }
    else if (t == VERSION) {
      r = version(b, 0);
    }
    else if (t == VERSION_CONSTRAINT) {
      r = versionConstraint(b, 0);
    }
    else {
      r = parse_root_(t, b, 0);
    }
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return cabal(b, l + 1);
  }

  /* ********************************************************** */
  // keyOrConfig*
  static boolean cabal(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cabal")) return false;
    int c = current_position_(b);
    while (true) {
      if (!keyOrConfig(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "cabal", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // WHITESPACERBRACETOK
  static boolean close(PsiBuilder b, int l) {
    return consumeToken(b, WHITESPACERBRACETOK);
  }

  /* ********************************************************** */
  // <<p>> (',' <<p>>)*
  static boolean commaSeparate(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = _p.parse(b, l);
    p = r; // pin = 1
    r = r && commaSeparate_1(b, l + 1, _p);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' <<p>>)*
  private static boolean commaSeparate_1(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!commaSeparate_1_0(b, l + 1, _p)) break;
      if (!empty_element_parsed_guard_(b, "commaSeparate_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' <<p>>
  private static boolean commaSeparate_1_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "commaSeparate_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, COMMA);
    p = r; // pin = 1
    r = r && _p.parse(b, l);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // extraSourceFilesKey colon <<commaSeparate varid>> |
  //                otherExtensionsKey colon <<commaSeparate varid>> |
  //                buildDependsKey colon <<commaSeparate dependency>> |
  //                exposedModulesKey colon <<commaSeparate  module >> |
  //                otherModulesKey colon <<commaSeparate  module >>
  public static boolean complexkey(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "complexkey")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<complexkey>");
    r = complexkey_0(b, l + 1);
    if (!r) r = complexkey_1(b, l + 1);
    if (!r) r = complexkey_2(b, l + 1);
    if (!r) r = complexkey_3(b, l + 1);
    if (!r) r = complexkey_4(b, l + 1);
    exit_section_(b, l, m, COMPLEXKEY, r, false, null);
    return r;
  }

  // extraSourceFilesKey colon <<commaSeparate varid>>
  private static boolean complexkey_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "complexkey_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTRASOURCEFILESKEY, COLON);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    exit_section_(b, m, null, r);
    return r;
  }

  // otherExtensionsKey colon <<commaSeparate varid>>
  private static boolean complexkey_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "complexkey_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, OTHEREXTENSIONSKEY, COLON);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    exit_section_(b, m, null, r);
    return r;
  }

  // buildDependsKey colon <<commaSeparate dependency>>
  private static boolean complexkey_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "complexkey_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BUILDDEPENDSKEY, COLON);
    r = r && commaSeparate(b, l + 1, dependency_parser_);
    exit_section_(b, m, null, r);
    return r;
  }

  // exposedModulesKey colon <<commaSeparate  module >>
  private static boolean complexkey_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "complexkey_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXPOSEDMODULESKEY, COLON);
    r = r && commaSeparate(b, l + 1, module_parser_);
    exit_section_(b, m, null, r);
    return r;
  }

  // otherModulesKey colon <<commaSeparate  module >>
  private static boolean complexkey_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "complexkey_4")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, OTHERMODULESKEY, COLON);
    r = r && commaSeparate(b, l + 1, module_parser_);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "if"
  public static boolean conditional(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conditional")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<conditional>");
    r = consumeToken(b, "if");
    exit_section_(b, l, m, CONDITIONAL, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // library | executable | flag
  public static boolean config(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<config>");
    r = library(b, l + 1);
    if (!r) r = executable(b, l + 1);
    if (!r) r = flag(b, l + 1);
    exit_section_(b, l, m, CONFIG, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // dependencyName [versionConstraint version [and versionConstraint version]]
  public static boolean dependency(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dependency")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = dependencyName(b, l + 1);
    r = r && dependency_1(b, l + 1);
    exit_section_(b, m, DEPENDENCY, r);
    return r;
  }

  // [versionConstraint version [and versionConstraint version]]
  private static boolean dependency_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dependency_1")) return false;
    dependency_1_0(b, l + 1);
    return true;
  }

  // versionConstraint version [and versionConstraint version]
  private static boolean dependency_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dependency_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = versionConstraint(b, l + 1);
    r = r && version(b, l + 1);
    r = r && dependency_1_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [and versionConstraint version]
  private static boolean dependency_1_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dependency_1_0_2")) return false;
    dependency_1_0_2_0(b, l + 1);
    return true;
  }

  // and versionConstraint version
  private static boolean dependency_1_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dependency_1_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, AND);
    r = r && versionConstraint(b, l + 1);
    r = r && version(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // varidRegexp
  public static boolean dependencyName(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dependencyName")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, VARIDREGEXP);
    exit_section_(b, m, DEPENDENCY_NAME, r);
    return r;
  }

  /* ********************************************************** */
  // <<p>> ('.' <<p>>)*
  static boolean dotSeparate(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "dotSeparate")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = _p.parse(b, l);
    p = r; // pin = 1
    r = r && dotSeparate_1(b, l + 1, _p);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ('.' <<p>>)*
  private static boolean dotSeparate_1(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "dotSeparate_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!dotSeparate_1_0(b, l + 1, _p)) break;
      if (!empty_element_parsed_guard_(b, "dotSeparate_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '.' <<p>>
  private static boolean dotSeparate_1_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "dotSeparate_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, DOT);
    p = r; // pin = 1
    r = r && _p.parse(b, l);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // "executable" varid open key+ close
  public static boolean executable(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "executable")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<executable>");
    r = consumeToken(b, "executable");
    r = r && varid(b, l + 1);
    r = r && open(b, l + 1);
    r = r && executable_3(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, l, m, EXECUTABLE, r, false, null);
    return r;
  }

  // key+
  private static boolean executable_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "executable_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = key(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!key(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "executable_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "flag" varid open key+ close
  public static boolean flag(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flag")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<flag>");
    r = consumeToken(b, "flag");
    r = r && varid(b, l + 1);
    r = r && open(b, l + 1);
    r = r && flag_3(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, l, m, FLAG, r, false, null);
    return r;
  }

  // key+
  private static boolean flag_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flag_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = key(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!key(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "flag_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // simplekey | complexkey
  public static boolean key(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "key")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<key>");
    r = simplekey(b, l + 1);
    if (!r) r = complexkey(b, l + 1);
    exit_section_(b, l, m, KEY, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // key | config
  public static boolean keyOrConfig(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "keyOrConfig")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<key or config>");
    r = key(b, l + 1);
    if (!r) r = config(b, l + 1);
    exit_section_(b, l, m, KEY_OR_CONFIG, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // "library" open key+ close
  public static boolean library(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "library")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<library>");
    r = consumeToken(b, "library");
    r = r && open(b, l + 1);
    r = r && library_2(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, l, m, LIBRARY, r, false, null);
    return r;
  }

  // key+
  private static boolean library_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "library_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = key(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!key(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "library_2", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<dotSeparate varid>>
  public static boolean module(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = dotSeparate(b, l + 1, varid_parser_);
    exit_section_(b, m, MODULE, r);
    return r;
  }

  /* ********************************************************** */
  // numberRegexp
  public static boolean number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "number")) return false;
    if (!nextTokenIs(b, NUMBERREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, NUMBERREGEXP);
    exit_section_(b, m, NUMBER, r);
    return r;
  }

  /* ********************************************************** */
  // WHITESPACELBRACETOK
  static boolean open(PsiBuilder b, int l) {
    return consumeToken(b, WHITESPACELBRACETOK);
  }

  /* ********************************************************** */
  // nameKey colon varid |
  //               synopsisKey colon varid |
  //               authorKey colon varid |
  //               maintainerKey colon varid |
  //               categoryKey colon varid |
  //               buildTypeKey colon varid |
  //               cabalVersionKey colon varid |
  //               defaultLanguageKey colon varid |
  //               versionKey colon version
  public static boolean simplekey(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<simplekey>");
    r = simplekey_0(b, l + 1);
    if (!r) r = simplekey_1(b, l + 1);
    if (!r) r = simplekey_2(b, l + 1);
    if (!r) r = simplekey_3(b, l + 1);
    if (!r) r = simplekey_4(b, l + 1);
    if (!r) r = simplekey_5(b, l + 1);
    if (!r) r = simplekey_6(b, l + 1);
    if (!r) r = simplekey_7(b, l + 1);
    if (!r) r = simplekey_8(b, l + 1);
    exit_section_(b, l, m, SIMPLEKEY, r, false, null);
    return r;
  }

  // nameKey colon varid
  private static boolean simplekey_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, NAMEKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // synopsisKey colon varid
  private static boolean simplekey_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, SYNOPSISKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // authorKey colon varid
  private static boolean simplekey_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, AUTHORKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // maintainerKey colon varid
  private static boolean simplekey_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, MAINTAINERKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // categoryKey colon varid
  private static boolean simplekey_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_4")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, CATEGORYKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // buildTypeKey colon varid
  private static boolean simplekey_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_5")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BUILDTYPEKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // cabalVersionKey colon varid
  private static boolean simplekey_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_6")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, CABALVERSIONKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // defaultLanguageKey colon varid
  private static boolean simplekey_7(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_7")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, DEFAULTLANGUAGEKEY, COLON);
    r = r && varid(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // versionKey colon version
  private static boolean simplekey_8(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simplekey_8")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, VERSIONKEY, COLON);
    r = r && version(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // varidRegexp
  public static boolean varid(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "varid")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, VARIDREGEXP);
    exit_section_(b, m, VARID, r);
    return r;
  }

  /* ********************************************************** */
  // <<dotSeparate number>>
  public static boolean version(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "version")) return false;
    if (!nextTokenIs(b, NUMBERREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = dotSeparate(b, l + 1, number_parser_);
    exit_section_(b, m, VERSION, r);
    return r;
  }

  /* ********************************************************** */
  // eq | gt | lt | gtEq | ltEq
  public static boolean versionConstraint(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "versionConstraint")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<version constraint>");
    r = consumeToken(b, EQ);
    if (!r) r = consumeToken(b, GT);
    if (!r) r = consumeToken(b, LT);
    if (!r) r = consumeToken(b, GTEQ);
    if (!r) r = consumeToken(b, LTEQ);
    exit_section_(b, l, m, VERSION_CONSTRAINT, r, false, null);
    return r;
  }

  final static Parser dependency_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return dependency(b, l + 1);
    }
  };
  final static Parser module_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return module(b, l + 1);
    }
  };
  final static Parser number_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return number(b, l + 1);
    }
  };
  final static Parser varid_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return varid(b, l + 1);
    }
  };
}
