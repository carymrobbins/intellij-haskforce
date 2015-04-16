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
    if (t == ADDRESS) {
      r = address(b, 0);
    }
    else if (t == AUTHOR) {
      r = author(b, 0);
    }
    else if (t == BENCHMARK) {
      r = benchmark(b, 0);
    }
    else if (t == BENCHMARK_KEYS) {
      r = benchmarkKeys(b, 0);
    }
    else if (t == BOOL) {
      r = bool(b, 0);
    }
    else if (t == BUG_REPORTS) {
      r = bugReports(b, 0);
    }
    else if (t == BUILD_DEPENDS) {
      r = buildDepends(b, 0);
    }
    else if (t == BUILD_INFORMATION) {
      r = buildInformation(b, 0);
    }
    else if (t == BUILD_TOOLS) {
      r = buildTools(b, 0);
    }
    else if (t == BUILD_TYPE) {
      r = buildType(b, 0);
    }
    else if (t == BUILD_TYPE_ENUM) {
      r = buildTypeEnum(b, 0);
    }
    else if (t == BUILDABLE) {
      r = buildable(b, 0);
    }
    else if (t == C_SOURCES) {
      r = cSources(b, 0);
    }
    else if (t == CABAL_PACKAGE) {
      r = cabalPackage(b, 0);
    }
    else if (t == CABAL_VERSION) {
      r = cabalVersion(b, 0);
    }
    else if (t == CATEGORY) {
      r = category(b, 0);
    }
    else if (t == CC_OPTIONS) {
      r = ccOptions(b, 0);
    }
    else if (t == COMPILER) {
      r = compiler(b, 0);
    }
    else if (t == CONDITION) {
      r = condition(b, 0);
    }
    else if (t == CONDITIONAL) {
      r = conditional(b, 0);
    }
    else if (t == CONDTIONAL_KEY) {
      r = condtionalKey(b, 0);
    }
    else if (t == CONFIG) {
      r = config(b, 0);
    }
    else if (t == COPYRIGHT) {
      r = copyright(b, 0);
    }
    else if (t == CPP_OPTIONS) {
      r = cppOptions(b, 0);
    }
    else if (t == DATA_DIR) {
      r = dataDir(b, 0);
    }
    else if (t == DATA_FILES) {
      r = dataFiles(b, 0);
    }
    else if (t == DEFAULT_LANGUAGE) {
      r = defaultLanguage(b, 0);
    }
    else if (t == DEPENDENCY) {
      r = dependency(b, 0);
    }
    else if (t == DEPENDENCY_NAME) {
      r = dependencyName(b, 0);
    }
    else if (t == DESCRIPTION) {
      r = description(b, 0);
    }
    else if (t == DIRECTORY) {
      r = directory(b, 0);
    }
    else if (t == EXECUTABLE) {
      r = executable(b, 0);
    }
    else if (t == EXECUTABLE_KEYS) {
      r = executableKeys(b, 0);
    }
    else if (t == EXECUTABLE_SPECIFIC_KEYS) {
      r = executableSpecificKeys(b, 0);
    }
    else if (t == EXPOSED) {
      r = exposed(b, 0);
    }
    else if (t == EXTENSIONS) {
      r = extensions(b, 0);
    }
    else if (t == EXTRA_DOC_FILES) {
      r = extraDocFiles(b, 0);
    }
    else if (t == EXTRA_GHCI_LIBRARIES) {
      r = extraGhciLibraries(b, 0);
    }
    else if (t == EXTRA_LIB_DIRS) {
      r = extraLibDirs(b, 0);
    }
    else if (t == EXTRA_LIBRARIES) {
      r = extraLibraries(b, 0);
    }
    else if (t == EXTRA_SOURCE_FILES) {
      r = extraSourceFiles(b, 0);
    }
    else if (t == EXTRA_TMP_FILES) {
      r = extraTmpFiles(b, 0);
    }
    else if (t == FILE_NAME) {
      r = fileName(b, 0);
    }
    else if (t == FILE_PATH) {
      r = filePath(b, 0);
    }
    else if (t == FLAG) {
      r = flag(b, 0);
    }
    else if (t == FLAG_KEYS) {
      r = flagKeys(b, 0);
    }
    else if (t == FRAMEWORKS) {
      r = frameworks(b, 0);
    }
    else if (t == FREEFORM) {
      r = freeform(b, 0);
    }
    else if (t == GHC_OPTION) {
      r = ghcOption(b, 0);
    }
    else if (t == GHC_OPTIONS) {
      r = ghcOptions(b, 0);
    }
    else if (t == GHC_PROF_OPTIONS) {
      r = ghcProfOptions(b, 0);
    }
    else if (t == GHC_SHARED_OPTIONS) {
      r = ghcSharedOptions(b, 0);
    }
    else if (t == HOMEPAGE) {
      r = homepage(b, 0);
    }
    else if (t == HS_SOURCE_DIRS) {
      r = hsSourceDirs(b, 0);
    }
    else if (t == INCLUDE_DIRS) {
      r = includeDirs(b, 0);
    }
    else if (t == INCLUDES) {
      r = includes(b, 0);
    }
    else if (t == INSTALL_INCLUDES) {
      r = installIncludes(b, 0);
    }
    else if (t == JS_SOURCES) {
      r = jsSources(b, 0);
    }
    else if (t == KEY) {
      r = key(b, 0);
    }
    else if (t == KEY_OR_CONFIG) {
      r = keyOrConfig(b, 0);
    }
    else if (t == LD_OPTIONS) {
      r = ldOptions(b, 0);
    }
    else if (t == LIBRARY) {
      r = library(b, 0);
    }
    else if (t == LIBRARY_KEYS) {
      r = libraryKeys(b, 0);
    }
    else if (t == LIBRARY_SPECIFIC_KEYS) {
      r = librarySpecificKeys(b, 0);
    }
    else if (t == LICENSE) {
      r = license(b, 0);
    }
    else if (t == LICENSE_FILE) {
      r = licenseFile(b, 0);
    }
    else if (t == LICENSE_FILES) {
      r = licenseFiles(b, 0);
    }
    else if (t == MAIN_IS) {
      r = mainIs(b, 0);
    }
    else if (t == MAINTAINER) {
      r = maintainer(b, 0);
    }
    else if (t == MODULE) {
      r = module(b, 0);
    }
    else if (t == NUMBER) {
      r = number(b, 0);
    }
    else if (t == OTHER_EXTENSIONS) {
      r = otherExtensions(b, 0);
    }
    else if (t == OTHER_MODULES) {
      r = otherModules(b, 0);
    }
    else if (t == PACKAGE_VERSION) {
      r = packageVersion(b, 0);
    }
    else if (t == PKG_CONFIG_DEPENDS) {
      r = pkgConfigDepends(b, 0);
    }
    else if (t == PROJECT_NAME) {
      r = projectName(b, 0);
    }
    else if (t == SOURCE_REPO_KEYS) {
      r = sourceRepoKeys(b, 0);
    }
    else if (t == SOURCE_REPOSITORY) {
      r = sourceRepository(b, 0);
    }
    else if (t == STABILITY) {
      r = stability(b, 0);
    }
    else if (t == SYNOPSIS) {
      r = synopsis(b, 0);
    }
    else if (t == TEST_INTERFACE) {
      r = testInterface(b, 0);
    }
    else if (t == TEST_MODULES) {
      r = testModules(b, 0);
    }
    else if (t == TEST_SUITE) {
      r = testSuite(b, 0);
    }
    else if (t == TEST_SUITE_KEYS) {
      r = testSuiteKeys(b, 0);
    }
    else if (t == TEST_SUITE_SPECIFIC_KEYS) {
      r = testSuiteSpecificKeys(b, 0);
    }
    else if (t == TEST_SUITE_TYPE) {
      r = testSuiteType(b, 0);
    }
    else if (t == TESTED_WITH) {
      r = testedWith(b, 0);
    }
    else if (t == URL) {
      r = url(b, 0);
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
  // freeformRegexp*
  public static boolean address(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "address")) return false;
    Marker m = enter_section_(b, l, _NONE_, "<address>");
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, FREEFORMREGEXP)) break;
      if (!empty_element_parsed_guard_(b, "address", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, l, m, ADDRESS, true, false, null);
    return true;
  }

  /* ********************************************************** */
  // authorKey colon (open freeform close)*
  public static boolean author(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "author")) return false;
    if (!nextTokenIs(b, AUTHORKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, AUTHORKEY, COLON);
    r = r && author_2(b, l + 1);
    exit_section_(b, m, AUTHOR, r);
    return r;
  }

  // (open freeform close)*
  private static boolean author_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "author_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!author_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "author_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean author_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "author_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "benchmark" varid open benchmarkKeys+ close
  public static boolean benchmark(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "benchmark")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<benchmark>");
    r = consumeToken(b, "benchmark");
    r = r && varid(b, l + 1);
    r = r && open(b, l + 1);
    r = r && benchmark_3(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, l, m, BENCHMARK, r, false, null);
    return r;
  }

  // benchmarkKeys+
  private static boolean benchmark_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "benchmark_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = benchmarkKeys(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!benchmarkKeys(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "benchmark_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // mainIs | testSuiteType | buildInformation | conditional
  public static boolean benchmarkKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "benchmarkKeys")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<benchmark keys>");
    r = mainIs(b, l + 1);
    if (!r) r = testSuiteType(b, l + 1);
    if (!r) r = buildInformation(b, l + 1);
    if (!r) r = conditional(b, l + 1);
    exit_section_(b, l, m, BENCHMARK_KEYS, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // true | false
  public static boolean bool(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bool")) return false;
    if (!nextTokenIs(b, "<bool>", FALSE, TRUE)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<bool>");
    r = consumeToken(b, TRUE);
    if (!r) r = consumeToken(b, FALSE);
    exit_section_(b, l, m, BOOL, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // bugReportsKey colon (open url close)*
  public static boolean bugReports(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bugReports")) return false;
    if (!nextTokenIs(b, BUGREPORTSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BUGREPORTSKEY, COLON);
    r = r && bugReports_2(b, l + 1);
    exit_section_(b, m, BUG_REPORTS, r);
    return r;
  }

  // (open url close)*
  private static boolean bugReports_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bugReports_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!bugReports_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "bugReports_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open url close
  private static boolean bugReports_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bugReports_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && url(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // buildDependsKey colon (open <<commaSeparate dependency>> close)*
  public static boolean buildDepends(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildDepends")) return false;
    if (!nextTokenIs(b, BUILDDEPENDSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BUILDDEPENDSKEY, COLON);
    r = r && buildDepends_2(b, l + 1);
    exit_section_(b, m, BUILD_DEPENDS, r);
    return r;
  }

  // (open <<commaSeparate dependency>> close)*
  private static boolean buildDepends_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildDepends_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!buildDepends_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "buildDepends_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate dependency>> close
  private static boolean buildDepends_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildDepends_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, dependency_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // buildDepends |
  //                      otherModules |
  //                      hsSourceDirs |
  //                      extensions |
  //                      buildTools |
  //                      buildable |
  //                      ghcOptions |
  //                      ghcProfOptions |
  //                      ghcSharedOptions |
  //                      includes |
  //                      installIncludes |
  //                      includeDirs |
  //                      cSources |
  //                      jsSources |
  //                      extraLibraries |
  //                      extraGhciLibraries |
  //                      extraLibDirs |
  //                      ccOptions |
  //                      cppOptions |
  //                      ldOptions |
  //                      pkgConfigDepends |
  //                      frameworks |
  //                      defaultLanguage
  public static boolean buildInformation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildInformation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<build information>");
    r = buildDepends(b, l + 1);
    if (!r) r = otherModules(b, l + 1);
    if (!r) r = hsSourceDirs(b, l + 1);
    if (!r) r = extensions(b, l + 1);
    if (!r) r = buildTools(b, l + 1);
    if (!r) r = buildable(b, l + 1);
    if (!r) r = ghcOptions(b, l + 1);
    if (!r) r = ghcProfOptions(b, l + 1);
    if (!r) r = ghcSharedOptions(b, l + 1);
    if (!r) r = includes(b, l + 1);
    if (!r) r = installIncludes(b, l + 1);
    if (!r) r = includeDirs(b, l + 1);
    if (!r) r = cSources(b, l + 1);
    if (!r) r = jsSources(b, l + 1);
    if (!r) r = extraLibraries(b, l + 1);
    if (!r) r = extraGhciLibraries(b, l + 1);
    if (!r) r = extraLibDirs(b, l + 1);
    if (!r) r = ccOptions(b, l + 1);
    if (!r) r = cppOptions(b, l + 1);
    if (!r) r = ldOptions(b, l + 1);
    if (!r) r = pkgConfigDepends(b, l + 1);
    if (!r) r = frameworks(b, l + 1);
    if (!r) r = defaultLanguage(b, l + 1);
    exit_section_(b, l, m, BUILD_INFORMATION, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // buildToolsKey colon (open <<commaSeparate dependency>> close)*
  public static boolean buildTools(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildTools")) return false;
    if (!nextTokenIs(b, BUILDTOOLSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BUILDTOOLSKEY, COLON);
    r = r && buildTools_2(b, l + 1);
    exit_section_(b, m, BUILD_TOOLS, r);
    return r;
  }

  // (open <<commaSeparate dependency>> close)*
  private static boolean buildTools_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildTools_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!buildTools_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "buildTools_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate dependency>> close
  private static boolean buildTools_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildTools_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, dependency_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // buildTypeKey colon (open buildTypeEnum close)*
  public static boolean buildType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildType")) return false;
    if (!nextTokenIs(b, BUILDTYPEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BUILDTYPEKEY, COLON);
    r = r && buildType_2(b, l + 1);
    exit_section_(b, m, BUILD_TYPE, r);
    return r;
  }

  // (open buildTypeEnum close)*
  private static boolean buildType_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildType_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!buildType_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "buildType_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open buildTypeEnum close
  private static boolean buildType_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildType_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && buildTypeEnum(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "simple" | "configure" | "make" | "custom"
  public static boolean buildTypeEnum(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildTypeEnum")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<build type enum>");
    r = consumeToken(b, "simple");
    if (!r) r = consumeToken(b, "configure");
    if (!r) r = consumeToken(b, "make");
    if (!r) r = consumeToken(b, "custom");
    exit_section_(b, l, m, BUILD_TYPE_ENUM, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // buildableKey colon (open bool close)*
  public static boolean buildable(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildable")) return false;
    if (!nextTokenIs(b, BUILDABLEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BUILDABLEKEY, COLON);
    r = r && buildable_2(b, l + 1);
    exit_section_(b, m, BUILDABLE, r);
    return r;
  }

  // (open bool close)*
  private static boolean buildable_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildable_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!buildable_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "buildable_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open bool close
  private static boolean buildable_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "buildable_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && bool(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // cSourcesKey colon (open <<commaSeparate fileName>> close)*
  public static boolean cSources(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cSources")) return false;
    if (!nextTokenIs(b, CSOURCESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, CSOURCESKEY, COLON);
    r = r && cSources_2(b, l + 1);
    exit_section_(b, m, C_SOURCES, r);
    return r;
  }

  // (open <<commaSeparate fileName>> close)*
  private static boolean cSources_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cSources_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!cSources_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "cSources_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate fileName>> close
  private static boolean cSources_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cSources_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, fileName_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
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
  // <<p>> (',' <<p>>)+ | <<p>>+
  static boolean cabalList(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "cabalList")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = cabalList_0(b, l + 1, _p);
    if (!r) r = cabalList_1(b, l + 1, _p);
    exit_section_(b, m, null, r);
    return r;
  }

  // <<p>> (',' <<p>>)+
  private static boolean cabalList_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "cabalList_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = _p.parse(b, l);
    r = r && cabalList_0_1(b, l + 1, _p);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' <<p>>)+
  private static boolean cabalList_0_1(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "cabalList_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = cabalList_0_1_0(b, l + 1, _p);
    int c = current_position_(b);
    while (r) {
      if (!cabalList_0_1_0(b, l + 1, _p)) break;
      if (!empty_element_parsed_guard_(b, "cabalList_0_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' <<p>>
  private static boolean cabalList_0_1_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "cabalList_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && _p.parse(b, l);
    exit_section_(b, m, null, r);
    return r;
  }

  // <<p>>+
  private static boolean cabalList_1(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "cabalList_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = _p.parse(b, l);
    int c = current_position_(b);
    while (r) {
      if (!_p.parse(b, l)) break;
      if (!empty_element_parsed_guard_(b, "cabalList_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // packageKey colon (open url close)*
  public static boolean cabalPackage(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cabalPackage")) return false;
    if (!nextTokenIs(b, PACKAGEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, PACKAGEKEY, COLON);
    r = r && cabalPackage_2(b, l + 1);
    exit_section_(b, m, CABAL_PACKAGE, r);
    return r;
  }

  // (open url close)*
  private static boolean cabalPackage_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cabalPackage_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!cabalPackage_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "cabalPackage_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open url close
  private static boolean cabalPackage_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cabalPackage_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && url(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // cabalVersionKey colon (open gtEq version close)*
  public static boolean cabalVersion(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cabalVersion")) return false;
    if (!nextTokenIs(b, CABALVERSIONKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, CABALVERSIONKEY, COLON);
    r = r && cabalVersion_2(b, l + 1);
    exit_section_(b, m, CABAL_VERSION, r);
    return r;
  }

  // (open gtEq version close)*
  private static boolean cabalVersion_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cabalVersion_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!cabalVersion_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "cabalVersion_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open gtEq version close
  private static boolean cabalVersion_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cabalVersion_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && consumeToken(b, GTEQ);
    r = r && version(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // categoryKey colon (open freeform close)*
  public static boolean category(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "category")) return false;
    if (!nextTokenIs(b, CATEGORYKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, CATEGORYKEY, COLON);
    r = r && category_2(b, l + 1);
    exit_section_(b, m, CATEGORY, r);
    return r;
  }

  // (open freeform close)*
  private static boolean category_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "category_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!category_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "category_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean category_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "category_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ccOptionsKey colon (open <<commaSeparate varid>> close)*
  public static boolean ccOptions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ccOptions")) return false;
    if (!nextTokenIs(b, CCOPTIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, CCOPTIONSKEY, COLON);
    r = r && ccOptions_2(b, l + 1);
    exit_section_(b, m, CC_OPTIONS, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean ccOptions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ccOptions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ccOptions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ccOptions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean ccOptions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ccOptions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // WHITESPACERBRACETOK
  static boolean close(PsiBuilder b, int l) {
    return consumeToken(b, WHITESPACERBRACETOK);
  }

  /* ********************************************************** */
  // <<p>> ( ',' <<p>>)*
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

  // ( ',' <<p>>)*
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
  // varidRegexp versionConstraint version
  public static boolean compiler(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compiler")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, VARIDREGEXP);
    r = r && versionConstraint(b, l + 1);
    r = r && version(b, l + 1);
    exit_section_(b, m, COMPILER, r);
    return r;
  }

  /* ********************************************************** */
  // freeformRegexp
  public static boolean condition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "condition")) return false;
    if (!nextTokenIs(b, FREEFORMREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, FREEFORMREGEXP);
    exit_section_(b, m, CONDITION, r);
    return r;
  }

  /* ********************************************************** */
  // if condition open condtionalKey+ close
  //   (else open condtionalKey+ close)*
  public static boolean conditional(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conditional")) return false;
    if (!nextTokenIs(b, IF)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, IF);
    r = r && condition(b, l + 1);
    r = r && open(b, l + 1);
    r = r && conditional_3(b, l + 1);
    r = r && close(b, l + 1);
    r = r && conditional_5(b, l + 1);
    exit_section_(b, m, CONDITIONAL, r);
    return r;
  }

  // condtionalKey+
  private static boolean conditional_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conditional_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = condtionalKey(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!condtionalKey(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "conditional_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // (else open condtionalKey+ close)*
  private static boolean conditional_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conditional_5")) return false;
    int c = current_position_(b);
    while (true) {
      if (!conditional_5_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "conditional_5", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // else open condtionalKey+ close
  private static boolean conditional_5_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conditional_5_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ELSE);
    r = r && open(b, l + 1);
    r = r && conditional_5_0_2(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // condtionalKey+
  private static boolean conditional_5_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "conditional_5_0_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = condtionalKey(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!condtionalKey(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "conditional_5_0_2", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // key | librarySpecificKeys | executableSpecificKeys | buildInformation
  public static boolean condtionalKey(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "condtionalKey")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<condtional key>");
    r = key(b, l + 1);
    if (!r) r = librarySpecificKeys(b, l + 1);
    if (!r) r = executableSpecificKeys(b, l + 1);
    if (!r) r = buildInformation(b, l + 1);
    exit_section_(b, l, m, CONDTIONAL_KEY, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // library | executable | testSuite | flag | sourceRepository | benchmark
  public static boolean config(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<config>");
    r = library(b, l + 1);
    if (!r) r = executable(b, l + 1);
    if (!r) r = testSuite(b, l + 1);
    if (!r) r = flag(b, l + 1);
    if (!r) r = sourceRepository(b, l + 1);
    if (!r) r = benchmark(b, l + 1);
    exit_section_(b, l, m, CONFIG, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // copyrightKey colon (open freeform close)*
  public static boolean copyright(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "copyright")) return false;
    if (!nextTokenIs(b, COPYRIGHTKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, COPYRIGHTKEY, COLON);
    r = r && copyright_2(b, l + 1);
    exit_section_(b, m, COPYRIGHT, r);
    return r;
  }

  // (open freeform close)*
  private static boolean copyright_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "copyright_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!copyright_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "copyright_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean copyright_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "copyright_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // cppOptionsKey colon (open freeform close)*
  public static boolean cppOptions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cppOptions")) return false;
    if (!nextTokenIs(b, CPPOPTIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, CPPOPTIONSKEY, COLON);
    r = r && cppOptions_2(b, l + 1);
    exit_section_(b, m, CPP_OPTIONS, r);
    return r;
  }

  // (open freeform close)*
  private static boolean cppOptions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cppOptions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!cppOptions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "cppOptions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean cppOptions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cppOptions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // dataDirKey colon (open directory close)*
  public static boolean dataDir(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dataDir")) return false;
    if (!nextTokenIs(b, DATADIRKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, DATADIRKEY, COLON);
    r = r && dataDir_2(b, l + 1);
    exit_section_(b, m, DATA_DIR, r);
    return r;
  }

  // (open directory close)*
  private static boolean dataDir_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dataDir_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!dataDir_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "dataDir_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open directory close
  private static boolean dataDir_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dataDir_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && directory(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // dataFilesKey colon (open <<commaSeparate fileName>> close)*
  public static boolean dataFiles(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dataFiles")) return false;
    if (!nextTokenIs(b, DATAFILESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, DATAFILESKEY, COLON);
    r = r && dataFiles_2(b, l + 1);
    exit_section_(b, m, DATA_FILES, r);
    return r;
  }

  // (open <<commaSeparate fileName>> close)*
  private static boolean dataFiles_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dataFiles_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!dataFiles_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "dataFiles_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate fileName>> close
  private static boolean dataFiles_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dataFiles_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, fileName_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // defaultLanguageKey colon (open varid close)*
  public static boolean defaultLanguage(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "defaultLanguage")) return false;
    if (!nextTokenIs(b, DEFAULTLANGUAGEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, DEFAULTLANGUAGEKEY, COLON);
    r = r && defaultLanguage_2(b, l + 1);
    exit_section_(b, m, DEFAULT_LANGUAGE, r);
    return r;
  }

  // (open varid close)*
  private static boolean defaultLanguage_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "defaultLanguage_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!defaultLanguage_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "defaultLanguage_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean defaultLanguage_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "defaultLanguage_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
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
  // descriptionKey colon (open freeform close)*
  public static boolean description(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "description")) return false;
    if (!nextTokenIs(b, DESCRIPTIONKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, DESCRIPTIONKEY, COLON);
    r = r && description_2(b, l + 1);
    exit_section_(b, m, DESCRIPTION, r);
    return r;
  }

  // (open freeform close)*
  private static boolean description_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "description_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!description_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "description_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean description_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "description_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<slashSeparate varid>>
  public static boolean directory(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "directory")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = slashSeparate(b, l + 1, varid_parser_);
    exit_section_(b, m, DIRECTORY, r);
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
  // "executable" varid open executableKeys+ close
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

  // executableKeys+
  private static boolean executable_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "executable_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = executableKeys(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!executableKeys(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "executable_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // executableSpecificKeys | conditional | buildInformation
  public static boolean executableKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "executableKeys")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<executable keys>");
    r = executableSpecificKeys(b, l + 1);
    if (!r) r = conditional(b, l + 1);
    if (!r) r = buildInformation(b, l + 1);
    exit_section_(b, l, m, EXECUTABLE_KEYS, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // mainIs
  public static boolean executableSpecificKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "executableSpecificKeys")) return false;
    if (!nextTokenIs(b, MAINISKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = mainIs(b, l + 1);
    exit_section_(b, m, EXECUTABLE_SPECIFIC_KEYS, r);
    return r;
  }

  /* ********************************************************** */
  // exposedKey colon (open bool close)*
  public static boolean exposed(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exposed")) return false;
    if (!nextTokenIs(b, EXPOSEDKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXPOSEDKEY, COLON);
    r = r && exposed_2(b, l + 1);
    exit_section_(b, m, EXPOSED, r);
    return r;
  }

  // (open bool close)*
  private static boolean exposed_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exposed_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!exposed_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "exposed_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open bool close
  private static boolean exposed_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exposed_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && bool(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // extensionsKey colon (open <<commaSeparate varid>> close)*
  public static boolean extensions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extensions")) return false;
    if (!nextTokenIs(b, EXTENSIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTENSIONSKEY, COLON);
    r = r && extensions_2(b, l + 1);
    exit_section_(b, m, EXTENSIONS, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean extensions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extensions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!extensions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extensions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean extensions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extensions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // extraDocFilesKey colon (open <<commaSeparate varid>> close)*
  public static boolean extraDocFiles(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraDocFiles")) return false;
    if (!nextTokenIs(b, EXTRADOCFILESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTRADOCFILESKEY, COLON);
    r = r && extraDocFiles_2(b, l + 1);
    exit_section_(b, m, EXTRA_DOC_FILES, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean extraDocFiles_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraDocFiles_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!extraDocFiles_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extraDocFiles_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean extraDocFiles_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraDocFiles_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // extraGhciLibrariesKey colon (open <<commaSeparate varid>> close)*
  public static boolean extraGhciLibraries(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraGhciLibraries")) return false;
    if (!nextTokenIs(b, EXTRAGHCILIBRARIESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTRAGHCILIBRARIESKEY, COLON);
    r = r && extraGhciLibraries_2(b, l + 1);
    exit_section_(b, m, EXTRA_GHCI_LIBRARIES, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean extraGhciLibraries_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraGhciLibraries_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!extraGhciLibraries_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extraGhciLibraries_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean extraGhciLibraries_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraGhciLibraries_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // extraLibDirsKey colon (open <<commaSeparate directory>> close)*
  public static boolean extraLibDirs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraLibDirs")) return false;
    if (!nextTokenIs(b, EXTRALIBDIRSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTRALIBDIRSKEY, COLON);
    r = r && extraLibDirs_2(b, l + 1);
    exit_section_(b, m, EXTRA_LIB_DIRS, r);
    return r;
  }

  // (open <<commaSeparate directory>> close)*
  private static boolean extraLibDirs_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraLibDirs_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!extraLibDirs_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extraLibDirs_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate directory>> close
  private static boolean extraLibDirs_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraLibDirs_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, directory_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // extraLibrariesKey colon (open <<commaSeparate varid>> close)*
  public static boolean extraLibraries(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraLibraries")) return false;
    if (!nextTokenIs(b, EXTRALIBRARIESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTRALIBRARIESKEY, COLON);
    r = r && extraLibraries_2(b, l + 1);
    exit_section_(b, m, EXTRA_LIBRARIES, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean extraLibraries_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraLibraries_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!extraLibraries_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extraLibraries_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean extraLibraries_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraLibraries_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // extraSourceFilesKey colon (open (<<commaSeparate varid >> | filePath*) close)*
  public static boolean extraSourceFiles(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraSourceFiles")) return false;
    if (!nextTokenIs(b, EXTRASOURCEFILESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTRASOURCEFILESKEY, COLON);
    r = r && extraSourceFiles_2(b, l + 1);
    exit_section_(b, m, EXTRA_SOURCE_FILES, r);
    return r;
  }

  // (open (<<commaSeparate varid >> | filePath*) close)*
  private static boolean extraSourceFiles_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraSourceFiles_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!extraSourceFiles_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extraSourceFiles_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open (<<commaSeparate varid >> | filePath*) close
  private static boolean extraSourceFiles_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraSourceFiles_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && extraSourceFiles_2_0_1(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // <<commaSeparate varid >> | filePath*
  private static boolean extraSourceFiles_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraSourceFiles_2_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = commaSeparate(b, l + 1, varid_parser_);
    if (!r) r = extraSourceFiles_2_0_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // filePath*
  private static boolean extraSourceFiles_2_0_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraSourceFiles_2_0_1_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!filePath(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extraSourceFiles_2_0_1_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // extraTmpFilesKey colon (open <<commaSeparate varid>> close)*
  public static boolean extraTmpFiles(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraTmpFiles")) return false;
    if (!nextTokenIs(b, EXTRATMPFILESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXTRATMPFILESKEY, COLON);
    r = r && extraTmpFiles_2(b, l + 1);
    exit_section_(b, m, EXTRA_TMP_FILES, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean extraTmpFiles_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraTmpFiles_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!extraTmpFiles_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "extraTmpFiles_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean extraTmpFiles_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "extraTmpFiles_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // varid "." varid
  public static boolean fileName(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fileName")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = varid(b, l + 1);
    r = r && consumeToken(b, DOT);
    r = r && varid(b, l + 1);
    exit_section_(b, m, FILE_NAME, r);
    return r;
  }

  /* ********************************************************** */
  // filePathRegexp
  public static boolean filePath(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "filePath")) return false;
    if (!nextTokenIs(b, FILEPATHREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, FILEPATHREGEXP);
    exit_section_(b, m, FILE_PATH, r);
    return r;
  }

  /* ********************************************************** */
  // "flag" varid open flagKeys+ close
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

  // flagKeys+
  private static boolean flag_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flag_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = flagKeys(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!flagKeys(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "flag_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // descriptionKey colon (open freeform close)* |
  //          defaultFlagValueKey colon (open bool close)* |
  //          manualKey colon (open bool close)*
  public static boolean flagKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<flag keys>");
    r = flagKeys_0(b, l + 1);
    if (!r) r = flagKeys_1(b, l + 1);
    if (!r) r = flagKeys_2(b, l + 1);
    exit_section_(b, l, m, FLAG_KEYS, r, false, null);
    return r;
  }

  // descriptionKey colon (open freeform close)*
  private static boolean flagKeys_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, DESCRIPTIONKEY, COLON);
    r = r && flagKeys_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open freeform close)*
  private static boolean flagKeys_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!flagKeys_0_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "flagKeys_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean flagKeys_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // defaultFlagValueKey colon (open bool close)*
  private static boolean flagKeys_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, DEFAULTFLAGVALUEKEY, COLON);
    r = r && flagKeys_1_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open bool close)*
  private static boolean flagKeys_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_1_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!flagKeys_1_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "flagKeys_1_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open bool close
  private static boolean flagKeys_1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && bool(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // manualKey colon (open bool close)*
  private static boolean flagKeys_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, MANUALKEY, COLON);
    r = r && flagKeys_2_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open bool close)*
  private static boolean flagKeys_2_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_2_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!flagKeys_2_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "flagKeys_2_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open bool close
  private static boolean flagKeys_2_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "flagKeys_2_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && bool(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // frameworksKey colon (open <<commaSeparate varid>> close)*
  public static boolean frameworks(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "frameworks")) return false;
    if (!nextTokenIs(b, FRAMEWORKSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, FRAMEWORKSKEY, COLON);
    r = r && frameworks_2(b, l + 1);
    exit_section_(b, m, FRAMEWORKS, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean frameworks_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "frameworks_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!frameworks_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "frameworks_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean frameworks_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "frameworks_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // freeformRegexp*
  public static boolean freeform(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "freeform")) return false;
    Marker m = enter_section_(b, l, _NONE_, "<freeform>");
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, FREEFORMREGEXP)) break;
      if (!empty_element_parsed_guard_(b, "freeform", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, l, m, FREEFORM, true, false, null);
    return true;
  }

  /* ********************************************************** */
  // varid (assign (varid | number))*
  public static boolean ghcOption(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOption")) return false;
    if (!nextTokenIs(b, VARIDREGEXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = varid(b, l + 1);
    r = r && ghcOption_1(b, l + 1);
    exit_section_(b, m, GHC_OPTION, r);
    return r;
  }

  // (assign (varid | number))*
  private static boolean ghcOption_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOption_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ghcOption_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ghcOption_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // assign (varid | number)
  private static boolean ghcOption_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOption_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ASSIGN);
    r = r && ghcOption_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // varid | number
  private static boolean ghcOption_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOption_1_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = varid(b, l + 1);
    if (!r) r = number(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ghcOptionsKey colon (open ghcOption* close)*
  public static boolean ghcOptions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOptions")) return false;
    if (!nextTokenIs(b, GHCOPTIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, GHCOPTIONSKEY, COLON);
    r = r && ghcOptions_2(b, l + 1);
    exit_section_(b, m, GHC_OPTIONS, r);
    return r;
  }

  // (open ghcOption* close)*
  private static boolean ghcOptions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOptions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ghcOptions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ghcOptions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open ghcOption* close
  private static boolean ghcOptions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOptions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && ghcOptions_2_0_1(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ghcOption*
  private static boolean ghcOptions_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcOptions_2_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ghcOption(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ghcOptions_2_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // ghcProfOptionsKey colon (open ghcOption* close)*
  public static boolean ghcProfOptions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcProfOptions")) return false;
    if (!nextTokenIs(b, GHCPROFOPTIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, GHCPROFOPTIONSKEY, COLON);
    r = r && ghcProfOptions_2(b, l + 1);
    exit_section_(b, m, GHC_PROF_OPTIONS, r);
    return r;
  }

  // (open ghcOption* close)*
  private static boolean ghcProfOptions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcProfOptions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ghcProfOptions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ghcProfOptions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open ghcOption* close
  private static boolean ghcProfOptions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcProfOptions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && ghcProfOptions_2_0_1(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ghcOption*
  private static boolean ghcProfOptions_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcProfOptions_2_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ghcOption(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ghcProfOptions_2_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // ghcSharedOptionsKey colon (open  ghcOption* close)*
  public static boolean ghcSharedOptions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcSharedOptions")) return false;
    if (!nextTokenIs(b, GHCSHAREDOPTIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, GHCSHAREDOPTIONSKEY, COLON);
    r = r && ghcSharedOptions_2(b, l + 1);
    exit_section_(b, m, GHC_SHARED_OPTIONS, r);
    return r;
  }

  // (open  ghcOption* close)*
  private static boolean ghcSharedOptions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcSharedOptions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ghcSharedOptions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ghcSharedOptions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open  ghcOption* close
  private static boolean ghcSharedOptions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcSharedOptions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && ghcSharedOptions_2_0_1(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ghcOption*
  private static boolean ghcSharedOptions_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ghcSharedOptions_2_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ghcOption(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ghcSharedOptions_2_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // homepageKey colon (open url close)*
  public static boolean homepage(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "homepage")) return false;
    if (!nextTokenIs(b, HOMEPAGEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, HOMEPAGEKEY, COLON);
    r = r && homepage_2(b, l + 1);
    exit_section_(b, m, HOMEPAGE, r);
    return r;
  }

  // (open url close)*
  private static boolean homepage_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "homepage_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!homepage_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "homepage_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open url close
  private static boolean homepage_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "homepage_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && url(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // hsSourceDirsKey colon (open <<cabalList directory>> close)*
  public static boolean hsSourceDirs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "hsSourceDirs")) return false;
    if (!nextTokenIs(b, HSSOURCEDIRSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, HSSOURCEDIRSKEY, COLON);
    r = r && hsSourceDirs_2(b, l + 1);
    exit_section_(b, m, HS_SOURCE_DIRS, r);
    return r;
  }

  // (open <<cabalList directory>> close)*
  private static boolean hsSourceDirs_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "hsSourceDirs_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!hsSourceDirs_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "hsSourceDirs_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<cabalList directory>> close
  private static boolean hsSourceDirs_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "hsSourceDirs_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && cabalList(b, l + 1, directory_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // includeDirsKey colon (open <<commaSeparate directory>> close)*
  public static boolean includeDirs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "includeDirs")) return false;
    if (!nextTokenIs(b, INCLUDEDIRSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, INCLUDEDIRSKEY, COLON);
    r = r && includeDirs_2(b, l + 1);
    exit_section_(b, m, INCLUDE_DIRS, r);
    return r;
  }

  // (open <<commaSeparate directory>> close)*
  private static boolean includeDirs_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "includeDirs_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!includeDirs_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "includeDirs_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate directory>> close
  private static boolean includeDirs_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "includeDirs_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, directory_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // includesKey colon (open <<commaSeparate fileName>> close)*
  public static boolean includes(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "includes")) return false;
    if (!nextTokenIs(b, INCLUDESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, INCLUDESKEY, COLON);
    r = r && includes_2(b, l + 1);
    exit_section_(b, m, INCLUDES, r);
    return r;
  }

  // (open <<commaSeparate fileName>> close)*
  private static boolean includes_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "includes_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!includes_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "includes_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate fileName>> close
  private static boolean includes_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "includes_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, fileName_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // installIncludesKey colon (open <<commaSeparate fileName>> close)*
  public static boolean installIncludes(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "installIncludes")) return false;
    if (!nextTokenIs(b, INSTALLINCLUDESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, INSTALLINCLUDESKEY, COLON);
    r = r && installIncludes_2(b, l + 1);
    exit_section_(b, m, INSTALL_INCLUDES, r);
    return r;
  }

  // (open <<commaSeparate fileName>> close)*
  private static boolean installIncludes_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "installIncludes_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!installIncludes_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "installIncludes_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate fileName>> close
  private static boolean installIncludes_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "installIncludes_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, fileName_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // jsSourcesKey colon (open <<commaSeparate fileName>> close)*
  public static boolean jsSources(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "jsSources")) return false;
    if (!nextTokenIs(b, JSSOURCESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, JSSOURCESKEY, COLON);
    r = r && jsSources_2(b, l + 1);
    exit_section_(b, m, JS_SOURCES, r);
    return r;
  }

  // (open <<commaSeparate fileName>> close)*
  private static boolean jsSources_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "jsSources_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!jsSources_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "jsSources_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate fileName>> close
  private static boolean jsSources_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "jsSources_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, fileName_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // projectName |
  //         synopsis |
  //         license |
  //         author |
  //         category |
  //         copyright |
  //         maintainer |
  //         buildType |
  //         stability|
  //         homepage |
  //         bugReports |
  //         cabalPackage |
  //         cabalVersion |
  //         licenseFile |
  //         description |
  //         dataDir|
  //         packageVersion |
  //         extraSourceFiles |
  //         extraDocFiles |
  //         extraTmpFiles |
  //         otherExtensions |
  //         testedWith |
  //         dataFiles |
  //         licenseFiles
  public static boolean key(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "key")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<key>");
    r = projectName(b, l + 1);
    if (!r) r = synopsis(b, l + 1);
    if (!r) r = license(b, l + 1);
    if (!r) r = author(b, l + 1);
    if (!r) r = category(b, l + 1);
    if (!r) r = copyright(b, l + 1);
    if (!r) r = maintainer(b, l + 1);
    if (!r) r = buildType(b, l + 1);
    if (!r) r = stability(b, l + 1);
    if (!r) r = homepage(b, l + 1);
    if (!r) r = bugReports(b, l + 1);
    if (!r) r = cabalPackage(b, l + 1);
    if (!r) r = cabalVersion(b, l + 1);
    if (!r) r = licenseFile(b, l + 1);
    if (!r) r = description(b, l + 1);
    if (!r) r = dataDir(b, l + 1);
    if (!r) r = packageVersion(b, l + 1);
    if (!r) r = extraSourceFiles(b, l + 1);
    if (!r) r = extraDocFiles(b, l + 1);
    if (!r) r = extraTmpFiles(b, l + 1);
    if (!r) r = otherExtensions(b, l + 1);
    if (!r) r = testedWith(b, l + 1);
    if (!r) r = dataFiles(b, l + 1);
    if (!r) r = licenseFiles(b, l + 1);
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
  // ldOptionsKey colon (open <<commaSeparate varid>> close)*
  public static boolean ldOptions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ldOptions")) return false;
    if (!nextTokenIs(b, LDOPTIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, LDOPTIONSKEY, COLON);
    r = r && ldOptions_2(b, l + 1);
    exit_section_(b, m, LD_OPTIONS, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean ldOptions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ldOptions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!ldOptions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ldOptions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean ldOptions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ldOptions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "library" open libraryKeys+ close
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

  // libraryKeys+
  private static boolean library_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "library_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = libraryKeys(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!libraryKeys(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "library_2", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // librarySpecificKeys | conditional | buildInformation
  public static boolean libraryKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "libraryKeys")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<library keys>");
    r = librarySpecificKeys(b, l + 1);
    if (!r) r = conditional(b, l + 1);
    if (!r) r = buildInformation(b, l + 1);
    exit_section_(b, l, m, LIBRARY_KEYS, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // exposedModulesKey colon (open <<cabalList module >> close)* |
  //                         exposed
  public static boolean librarySpecificKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "librarySpecificKeys")) return false;
    if (!nextTokenIs(b, "<library specific keys>", EXPOSEDKEY, EXPOSEDMODULESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<library specific keys>");
    r = librarySpecificKeys_0(b, l + 1);
    if (!r) r = exposed(b, l + 1);
    exit_section_(b, l, m, LIBRARY_SPECIFIC_KEYS, r, false, null);
    return r;
  }

  // exposedModulesKey colon (open <<cabalList module >> close)*
  private static boolean librarySpecificKeys_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "librarySpecificKeys_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, EXPOSEDMODULESKEY, COLON);
    r = r && librarySpecificKeys_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open <<cabalList module >> close)*
  private static boolean librarySpecificKeys_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "librarySpecificKeys_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!librarySpecificKeys_0_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "librarySpecificKeys_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<cabalList module >> close
  private static boolean librarySpecificKeys_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "librarySpecificKeys_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && cabalList(b, l + 1, module_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // licenseKey colon (open varid close)*
  public static boolean license(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "license")) return false;
    if (!nextTokenIs(b, LICENSEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, LICENSEKEY, COLON);
    r = r && license_2(b, l + 1);
    exit_section_(b, m, LICENSE, r);
    return r;
  }

  // (open varid close)*
  private static boolean license_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "license_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!license_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "license_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean license_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "license_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // licenseFileKey colon (open freeform close)*
  public static boolean licenseFile(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "licenseFile")) return false;
    if (!nextTokenIs(b, LICENSEFILEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, LICENSEFILEKEY, COLON);
    r = r && licenseFile_2(b, l + 1);
    exit_section_(b, m, LICENSE_FILE, r);
    return r;
  }

  // (open freeform close)*
  private static boolean licenseFile_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "licenseFile_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!licenseFile_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "licenseFile_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean licenseFile_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "licenseFile_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // licenseFilesKey colon (open <<commaSeparate fileName>> close)*
  public static boolean licenseFiles(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "licenseFiles")) return false;
    if (!nextTokenIs(b, LICENSEFILESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, LICENSEFILESKEY, COLON);
    r = r && licenseFiles_2(b, l + 1);
    exit_section_(b, m, LICENSE_FILES, r);
    return r;
  }

  // (open <<commaSeparate fileName>> close)*
  private static boolean licenseFiles_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "licenseFiles_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!licenseFiles_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "licenseFiles_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate fileName>> close
  private static boolean licenseFiles_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "licenseFiles_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, fileName_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // mainIsKey colon (open filePath close)*
  public static boolean mainIs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "mainIs")) return false;
    if (!nextTokenIs(b, MAINISKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, MAINISKEY, COLON);
    r = r && mainIs_2(b, l + 1);
    exit_section_(b, m, MAIN_IS, r);
    return r;
  }

  // (open filePath close)*
  private static boolean mainIs_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "mainIs_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!mainIs_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "mainIs_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open filePath close
  private static boolean mainIs_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "mainIs_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && filePath(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // maintainerKey colon (open address close)*
  public static boolean maintainer(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "maintainer")) return false;
    if (!nextTokenIs(b, MAINTAINERKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, MAINTAINERKEY, COLON);
    r = r && maintainer_2(b, l + 1);
    exit_section_(b, m, MAINTAINER, r);
    return r;
  }

  // (open address close)*
  private static boolean maintainer_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "maintainer_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!maintainer_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "maintainer_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open address close
  private static boolean maintainer_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "maintainer_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && address(b, l + 1);
    r = r && close(b, l + 1);
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
  // otherExtensionsKey colon (open <<commaSeparate varid>> close)*
  public static boolean otherExtensions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "otherExtensions")) return false;
    if (!nextTokenIs(b, OTHEREXTENSIONSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, OTHEREXTENSIONSKEY, COLON);
    r = r && otherExtensions_2(b, l + 1);
    exit_section_(b, m, OTHER_EXTENSIONS, r);
    return r;
  }

  // (open <<commaSeparate varid>> close)*
  private static boolean otherExtensions_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "otherExtensions_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!otherExtensions_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "otherExtensions_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate varid>> close
  private static boolean otherExtensions_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "otherExtensions_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, varid_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // otherModulesKey colon (open <<cabalList module >> close)*
  public static boolean otherModules(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "otherModules")) return false;
    if (!nextTokenIs(b, OTHERMODULESKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, OTHERMODULESKEY, COLON);
    r = r && otherModules_2(b, l + 1);
    exit_section_(b, m, OTHER_MODULES, r);
    return r;
  }

  // (open <<cabalList module >> close)*
  private static boolean otherModules_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "otherModules_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!otherModules_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "otherModules_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<cabalList module >> close
  private static boolean otherModules_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "otherModules_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && cabalList(b, l + 1, module_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // versionKey colon (open version close)*
  public static boolean packageVersion(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "packageVersion")) return false;
    if (!nextTokenIs(b, VERSIONKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, VERSIONKEY, COLON);
    r = r && packageVersion_2(b, l + 1);
    exit_section_(b, m, PACKAGE_VERSION, r);
    return r;
  }

  // (open version close)*
  private static boolean packageVersion_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "packageVersion_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!packageVersion_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "packageVersion_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open version close
  private static boolean packageVersion_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "packageVersion_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && version(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // pkgConfigDependsKey colon (open <<commaSeparate dependency>> close)*
  public static boolean pkgConfigDepends(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkgConfigDepends")) return false;
    if (!nextTokenIs(b, PKGCONFIGDEPENDSKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, PKGCONFIGDEPENDSKEY, COLON);
    r = r && pkgConfigDepends_2(b, l + 1);
    exit_section_(b, m, PKG_CONFIG_DEPENDS, r);
    return r;
  }

  // (open <<commaSeparate dependency>> close)*
  private static boolean pkgConfigDepends_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkgConfigDepends_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!pkgConfigDepends_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "pkgConfigDepends_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate dependency>> close
  private static boolean pkgConfigDepends_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pkgConfigDepends_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, dependency_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // nameKey colon (open varid close)*
  public static boolean projectName(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "projectName")) return false;
    if (!nextTokenIs(b, NAMEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, NAMEKEY, COLON);
    r = r && projectName_2(b, l + 1);
    exit_section_(b, m, PROJECT_NAME, r);
    return r;
  }

  // (open varid close)*
  private static boolean projectName_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "projectName_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!projectName_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "projectName_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean projectName_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "projectName_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<p>> ('/' <<p>>)*
  static boolean slashSeparate(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "slashSeparate")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = _p.parse(b, l);
    p = r; // pin = 1
    r = r && slashSeparate_1(b, l + 1, _p);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ('/' <<p>>)*
  private static boolean slashSeparate_1(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "slashSeparate_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!slashSeparate_1_0(b, l + 1, _p)) break;
      if (!empty_element_parsed_guard_(b, "slashSeparate_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '/' <<p>>
  private static boolean slashSeparate_1_0(PsiBuilder b, int l, final Parser _p) {
    if (!recursion_guard_(b, l, "slashSeparate_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, SLASH);
    p = r; // pin = 1
    r = r && _p.parse(b, l);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // typeKey colon (open varid close)* |
  //                    locationKey colon (open freeform close)* |
  //                    moduleKey colon (open varid close)* |
  //                    branchKey colon (open varid close)* |
  //                    tagKey colon (open varid close)* |
  //                    subDirKey colon (open varid close)*
  public static boolean sourceRepoKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<source repo keys>");
    r = sourceRepoKeys_0(b, l + 1);
    if (!r) r = sourceRepoKeys_1(b, l + 1);
    if (!r) r = sourceRepoKeys_2(b, l + 1);
    if (!r) r = sourceRepoKeys_3(b, l + 1);
    if (!r) r = sourceRepoKeys_4(b, l + 1);
    if (!r) r = sourceRepoKeys_5(b, l + 1);
    exit_section_(b, l, m, SOURCE_REPO_KEYS, r, false, null);
    return r;
  }

  // typeKey colon (open varid close)*
  private static boolean sourceRepoKeys_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, TYPEKEY, COLON);
    r = r && sourceRepoKeys_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open varid close)*
  private static boolean sourceRepoKeys_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!sourceRepoKeys_0_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sourceRepoKeys_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean sourceRepoKeys_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // locationKey colon (open freeform close)*
  private static boolean sourceRepoKeys_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, LOCATIONKEY, COLON);
    r = r && sourceRepoKeys_1_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open freeform close)*
  private static boolean sourceRepoKeys_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_1_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!sourceRepoKeys_1_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sourceRepoKeys_1_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean sourceRepoKeys_1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // moduleKey colon (open varid close)*
  private static boolean sourceRepoKeys_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, MODULEKEY, COLON);
    r = r && sourceRepoKeys_2_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open varid close)*
  private static boolean sourceRepoKeys_2_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_2_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!sourceRepoKeys_2_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sourceRepoKeys_2_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean sourceRepoKeys_2_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_2_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // branchKey colon (open varid close)*
  private static boolean sourceRepoKeys_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, BRANCHKEY, COLON);
    r = r && sourceRepoKeys_3_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open varid close)*
  private static boolean sourceRepoKeys_3_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_3_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!sourceRepoKeys_3_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sourceRepoKeys_3_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean sourceRepoKeys_3_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_3_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // tagKey colon (open varid close)*
  private static boolean sourceRepoKeys_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_4")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, TAGKEY, COLON);
    r = r && sourceRepoKeys_4_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open varid close)*
  private static boolean sourceRepoKeys_4_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_4_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!sourceRepoKeys_4_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sourceRepoKeys_4_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean sourceRepoKeys_4_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_4_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // subDirKey colon (open varid close)*
  private static boolean sourceRepoKeys_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_5")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, SUBDIRKEY, COLON);
    r = r && sourceRepoKeys_5_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (open varid close)*
  private static boolean sourceRepoKeys_5_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_5_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!sourceRepoKeys_5_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sourceRepoKeys_5_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean sourceRepoKeys_5_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepoKeys_5_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // sourceRepositoryKey varid open sourceRepoKeys+ close
  public static boolean sourceRepository(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepository")) return false;
    if (!nextTokenIs(b, SOURCEREPOSITORYKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SOURCEREPOSITORYKEY);
    r = r && varid(b, l + 1);
    r = r && open(b, l + 1);
    r = r && sourceRepository_3(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, SOURCE_REPOSITORY, r);
    return r;
  }

  // sourceRepoKeys+
  private static boolean sourceRepository_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sourceRepository_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = sourceRepoKeys(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!sourceRepoKeys(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sourceRepository_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // stabilityKey colon (open varid close)*
  public static boolean stability(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stability")) return false;
    if (!nextTokenIs(b, STABILITYKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, STABILITYKEY, COLON);
    r = r && stability_2(b, l + 1);
    exit_section_(b, m, STABILITY, r);
    return r;
  }

  // (open varid close)*
  private static boolean stability_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stability_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!stability_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "stability_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean stability_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stability_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // synopsisKey colon (open freeform close)*
  public static boolean synopsis(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "synopsis")) return false;
    if (!nextTokenIs(b, SYNOPSISKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, SYNOPSISKEY, COLON);
    r = r && synopsis_2(b, l + 1);
    exit_section_(b, m, SYNOPSIS, r);
    return r;
  }

  // (open freeform close)*
  private static boolean synopsis_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "synopsis_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!synopsis_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "synopsis_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open freeform close
  private static boolean synopsis_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "synopsis_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && freeform(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "exitcode-stdio-1.0" | "detailed-0.9"
  public static boolean testInterface(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testInterface")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<test interface>");
    r = consumeToken(b, "exitcode-stdio-1.0");
    if (!r) r = consumeToken(b, "detailed-0.9");
    exit_section_(b, l, m, TEST_INTERFACE, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // testModuleKey colon (open varid close)*
  public static boolean testModules(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testModules")) return false;
    if (!nextTokenIs(b, TESTMODULEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, TESTMODULEKEY, COLON);
    r = r && testModules_2(b, l + 1);
    exit_section_(b, m, TEST_MODULES, r);
    return r;
  }

  // (open varid close)*
  private static boolean testModules_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testModules_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!testModules_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "testModules_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open varid close
  private static boolean testModules_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testModules_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && varid(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // "test-suite" varid open testSuiteKeys+ close
  public static boolean testSuite(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testSuite")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<test suite>");
    r = consumeToken(b, "test-suite");
    r = r && varid(b, l + 1);
    r = r && open(b, l + 1);
    r = r && testSuite_3(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, l, m, TEST_SUITE, r, false, null);
    return r;
  }

  // testSuiteKeys+
  private static boolean testSuite_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testSuite_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = testSuiteKeys(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!testSuiteKeys(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "testSuite_3", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // testSuiteSpecificKeys | buildInformation | conditional
  public static boolean testSuiteKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testSuiteKeys")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<test suite keys>");
    r = testSuiteSpecificKeys(b, l + 1);
    if (!r) r = buildInformation(b, l + 1);
    if (!r) r = conditional(b, l + 1);
    exit_section_(b, l, m, TEST_SUITE_KEYS, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // testSuiteType |
  //                           mainIs |
  //                           testModules
  public static boolean testSuiteSpecificKeys(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testSuiteSpecificKeys")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<test suite specific keys>");
    r = testSuiteType(b, l + 1);
    if (!r) r = mainIs(b, l + 1);
    if (!r) r = testModules(b, l + 1);
    exit_section_(b, l, m, TEST_SUITE_SPECIFIC_KEYS, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // typeKey colon (open testInterface close)*
  public static boolean testSuiteType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testSuiteType")) return false;
    if (!nextTokenIs(b, TYPEKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, TYPEKEY, COLON);
    r = r && testSuiteType_2(b, l + 1);
    exit_section_(b, m, TEST_SUITE_TYPE, r);
    return r;
  }

  // (open testInterface close)*
  private static boolean testSuiteType_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testSuiteType_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!testSuiteType_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "testSuiteType_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open testInterface close
  private static boolean testSuiteType_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testSuiteType_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && testInterface(b, l + 1);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // testedWithKey colon (open <<commaSeparate compiler>> close)*
  public static boolean testedWith(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testedWith")) return false;
    if (!nextTokenIs(b, TESTEDWITHKEY)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, TESTEDWITHKEY, COLON);
    r = r && testedWith_2(b, l + 1);
    exit_section_(b, m, TESTED_WITH, r);
    return r;
  }

  // (open <<commaSeparate compiler>> close)*
  private static boolean testedWith_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testedWith_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!testedWith_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "testedWith_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // open <<commaSeparate compiler>> close
  private static boolean testedWith_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "testedWith_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = open(b, l + 1);
    r = r && commaSeparate(b, l + 1, compiler_parser_);
    r = r && close(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // freeformRegexp*
  public static boolean url(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "url")) return false;
    Marker m = enter_section_(b, l, _NONE_, "<url>");
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, FREEFORMREGEXP)) break;
      if (!empty_element_parsed_guard_(b, "url", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, l, m, URL, true, false, null);
    return true;
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

  final static Parser compiler_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return compiler(b, l + 1);
    }
  };
  final static Parser dependency_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return dependency(b, l + 1);
    }
  };
  final static Parser directory_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return directory(b, l + 1);
    }
  };
  final static Parser fileName_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return fileName(b, l + 1);
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
