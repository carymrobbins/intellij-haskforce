// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.haskforce.cabal.psi.impl.*;

public interface CabalTypes {

  IElementType ADDRESS = new CabalElementType("ADDRESS");
  IElementType AUTHOR = new CabalElementType("AUTHOR");
  IElementType BENCHMARK = new CabalElementType("BENCHMARK");
  IElementType BENCHMARK_KEYS = new CabalElementType("BENCHMARK_KEYS");
  IElementType BOOL = new CabalElementType("BOOL");
  IElementType BUG_REPORTS = new CabalElementType("BUG_REPORTS");
  IElementType BUILDABLE = new CabalElementType("BUILDABLE");
  IElementType BUILD_DEPENDS = new CabalElementType("BUILD_DEPENDS");
  IElementType BUILD_INFORMATION = new CabalElementType("BUILD_INFORMATION");
  IElementType BUILD_TOOLS = new CabalElementType("BUILD_TOOLS");
  IElementType BUILD_TYPE = new CabalElementType("BUILD_TYPE");
  IElementType BUILD_TYPE_ENUM = new CabalElementType("BUILD_TYPE_ENUM");
  IElementType CABAL_PACKAGE = new CabalElementType("CABAL_PACKAGE");
  IElementType CABAL_VERSION = new CabalElementType("CABAL_VERSION");
  IElementType CATEGORY = new CabalElementType("CATEGORY");
  IElementType CC_OPTIONS = new CabalElementType("CC_OPTIONS");
  IElementType COMPILER = new CabalElementType("COMPILER");
  IElementType CONDITION = new CabalElementType("CONDITION");
  IElementType CONDITIONAL = new CabalElementType("CONDITIONAL");
  IElementType CONDTIONAL_KEY = new CabalElementType("CONDTIONAL_KEY");
  IElementType CONFIG = new CabalElementType("CONFIG");
  IElementType COPYRIGHT = new CabalElementType("COPYRIGHT");
  IElementType CPP_OPTIONS = new CabalElementType("CPP_OPTIONS");
  IElementType C_SOURCES = new CabalElementType("C_SOURCES");
  IElementType DATA_DIR = new CabalElementType("DATA_DIR");
  IElementType DATA_FILES = new CabalElementType("DATA_FILES");
  IElementType DEFAULT_LANGUAGE = new CabalElementType("DEFAULT_LANGUAGE");
  IElementType DEPENDENCY = new CabalElementType("DEPENDENCY");
  IElementType DEPENDENCY_NAME = new CabalElementType("DEPENDENCY_NAME");
  IElementType DESCRIPTION = new CabalElementType("DESCRIPTION");
  IElementType DIRECTORY = new CabalElementType("DIRECTORY");
  IElementType EXECUTABLE = new CabalElementType("EXECUTABLE");
  IElementType EXECUTABLE_KEYS = new CabalElementType("EXECUTABLE_KEYS");
  IElementType EXECUTABLE_SPECIFIC_KEYS = new CabalElementType("EXECUTABLE_SPECIFIC_KEYS");
  IElementType EXPOSED = new CabalElementType("EXPOSED");
  IElementType EXTENSIONS = new CabalElementType("EXTENSIONS");
  IElementType EXTRA_DOC_FILES = new CabalElementType("EXTRA_DOC_FILES");
  IElementType EXTRA_GHCI_LIBRARIES = new CabalElementType("EXTRA_GHCI_LIBRARIES");
  IElementType EXTRA_LIBRARIES = new CabalElementType("EXTRA_LIBRARIES");
  IElementType EXTRA_LIB_DIRS = new CabalElementType("EXTRA_LIB_DIRS");
  IElementType EXTRA_SOURCE_FILES = new CabalElementType("EXTRA_SOURCE_FILES");
  IElementType EXTRA_TMP_FILES = new CabalElementType("EXTRA_TMP_FILES");
  IElementType FILE_NAME = new CabalElementType("FILE_NAME");
  IElementType FILE_PATH = new CabalElementType("FILE_PATH");
  IElementType FLAG = new CabalElementType("FLAG");
  IElementType FLAG_KEYS = new CabalElementType("FLAG_KEYS");
  IElementType FRAMEWORKS = new CabalElementType("FRAMEWORKS");
  IElementType FREEFORM = new CabalElementType("FREEFORM");
  IElementType GHC_OPTION = new CabalElementType("GHC_OPTION");
  IElementType GHC_OPTIONS = new CabalElementType("GHC_OPTIONS");
  IElementType GHC_PROF_OPTIONS = new CabalElementType("GHC_PROF_OPTIONS");
  IElementType GHC_SHARED_OPTIONS = new CabalElementType("GHC_SHARED_OPTIONS");
  IElementType HOMEPAGE = new CabalElementType("HOMEPAGE");
  IElementType HS_SOURCE_DIRS = new CabalElementType("HS_SOURCE_DIRS");
  IElementType INCLUDES = new CabalElementType("INCLUDES");
  IElementType INCLUDE_DIRS = new CabalElementType("INCLUDE_DIRS");
  IElementType INSTALL_INCLUDES = new CabalElementType("INSTALL_INCLUDES");
  IElementType JS_SOURCES = new CabalElementType("JS_SOURCES");
  IElementType KEY = new CabalElementType("KEY");
  IElementType KEY_OR_CONFIG = new CabalElementType("KEY_OR_CONFIG");
  IElementType LD_OPTIONS = new CabalElementType("LD_OPTIONS");
  IElementType LIBRARY = new CabalElementType("LIBRARY");
  IElementType LIBRARY_KEYS = new CabalElementType("LIBRARY_KEYS");
  IElementType LIBRARY_SPECIFIC_KEYS = new CabalElementType("LIBRARY_SPECIFIC_KEYS");
  IElementType LICENSE = new CabalElementType("LICENSE");
  IElementType LICENSE_FILE = new CabalElementType("LICENSE_FILE");
  IElementType LICENSE_FILES = new CabalElementType("LICENSE_FILES");
  IElementType MAINTAINER = new CabalElementType("MAINTAINER");
  IElementType MAIN_IS = new CabalElementType("MAIN_IS");
  IElementType MODULE = new CabalElementType("MODULE");
  IElementType NUMBER = new CabalElementType("NUMBER");
  IElementType OTHER_EXTENSIONS = new CabalElementType("OTHER_EXTENSIONS");
  IElementType OTHER_MODULES = new CabalElementType("OTHER_MODULES");
  IElementType PACKAGE_VERSION = new CabalElementType("PACKAGE_VERSION");
  IElementType PKG_CONFIG_DEPENDS = new CabalElementType("PKG_CONFIG_DEPENDS");
  IElementType PROJECT_NAME = new CabalElementType("PROJECT_NAME");
  IElementType SOURCE_REPOSITORY = new CabalElementType("SOURCE_REPOSITORY");
  IElementType SOURCE_REPO_KEYS = new CabalElementType("SOURCE_REPO_KEYS");
  IElementType STABILITY = new CabalElementType("STABILITY");
  IElementType SYNOPSIS = new CabalElementType("SYNOPSIS");
  IElementType TESTED_WITH = new CabalElementType("TESTED_WITH");
  IElementType TEST_INTERFACE = new CabalElementType("TEST_INTERFACE");
  IElementType TEST_MODULES = new CabalElementType("TEST_MODULES");
  IElementType TEST_SUITE = new CabalElementType("TEST_SUITE");
  IElementType TEST_SUITE_KEYS = new CabalElementType("TEST_SUITE_KEYS");
  IElementType TEST_SUITE_SPECIFIC_KEYS = new CabalElementType("TEST_SUITE_SPECIFIC_KEYS");
  IElementType TEST_SUITE_TYPE = new CabalElementType("TEST_SUITE_TYPE");
  IElementType URL = new CabalElementType("URL");
  IElementType VARID = new CabalElementType("VARID");
  IElementType VERSION = new CabalElementType("VERSION");
  IElementType VERSION_CONSTRAINT = new CabalElementType("VERSION_CONSTRAINT");

  IElementType AND = new CabalTokenType("&&");
  IElementType ASSIGN = new CabalTokenType("=");
  IElementType AUTHORKEY = new CabalTokenType("author");
  IElementType BRANCHKEY = new CabalTokenType("branch");
  IElementType BUGREPORTSKEY = new CabalTokenType("bug-reports");
  IElementType BUILDABLEKEY = new CabalTokenType("buildable");
  IElementType BUILDDEPENDSKEY = new CabalTokenType("build-depends");
  IElementType BUILDTOOLSKEY = new CabalTokenType("build-tools");
  IElementType BUILDTYPEKEY = new CabalTokenType("build-type");
  IElementType CABALVERSIONKEY = new CabalTokenType("cabal-version");
  IElementType CATEGORYKEY = new CabalTokenType("category");
  IElementType CCOPTIONSKEY = new CabalTokenType("cc-options");
  IElementType COLON = new CabalTokenType(":");
  IElementType COMMA = new CabalTokenType(",");
  IElementType COMMENT = new CabalTokenType("comment");
  IElementType CONDITIONREGEXP = new CabalTokenType("conditionRegexp");
  IElementType COPYRIGHTKEY = new CabalTokenType("copyright");
  IElementType CPPOPTIONSKEY = new CabalTokenType("cpp-options");
  IElementType CSOURCESKEY = new CabalTokenType("c-sources");
  IElementType DATADIRKEY = new CabalTokenType("data-dir");
  IElementType DATAFILESKEY = new CabalTokenType("data-files");
  IElementType DEFAULTFLAGVALUEKEY = new CabalTokenType("default");
  IElementType DEFAULTLANGUAGEKEY = new CabalTokenType("default-language");
  IElementType DESCRIPTIONKEY = new CabalTokenType("description");
  IElementType DOT = new CabalTokenType(".");
  IElementType ELSE = new CabalTokenType("else");
  IElementType EQ = new CabalTokenType("==");
  IElementType EXPOSEDKEY = new CabalTokenType("exposed");
  IElementType EXPOSEDMODULESKEY = new CabalTokenType("exposed-modules");
  IElementType EXTENSIONSKEY = new CabalTokenType("extensions");
  IElementType EXTRADOCFILESKEY = new CabalTokenType("extra-doc-files");
  IElementType EXTRAGHCILIBRARIESKEY = new CabalTokenType("extra-ghci-libraries");
  IElementType EXTRALIBDIRSKEY = new CabalTokenType("extra-lib-dirs");
  IElementType EXTRALIBRARIESKEY = new CabalTokenType("extra-libraries");
  IElementType EXTRASOURCEFILESKEY = new CabalTokenType("extra-source-files");
  IElementType EXTRATMPFILESKEY = new CabalTokenType("extra-tmp-files");
  IElementType FALSE = new CabalTokenType("false");
  IElementType FILEPATHREGEXP = new CabalTokenType("filePathRegexp");
  IElementType FRAMEWORKSKEY = new CabalTokenType("frameworks");
  IElementType FREEFORMREGEXP = new CabalTokenType("freeformRegexp");
  IElementType GHCOPTIONSKEY = new CabalTokenType("ghc-options");
  IElementType GHCPROFOPTIONSKEY = new CabalTokenType("ghc-prof-options");
  IElementType GHCSHAREDOPTIONSKEY = new CabalTokenType("ghc-shared-options");
  IElementType GT = new CabalTokenType(">");
  IElementType GTEQ = new CabalTokenType(">=");
  IElementType HOMEPAGEKEY = new CabalTokenType("homepage");
  IElementType HSSOURCEDIRSKEY = new CabalTokenType("hs-source-dirs");
  IElementType IF = new CabalTokenType("if");
  IElementType INCLUDEDIRSKEY = new CabalTokenType("include-dirs");
  IElementType INCLUDESKEY = new CabalTokenType("includes");
  IElementType INSTALLINCLUDESKEY = new CabalTokenType("install-includes");
  IElementType JSSOURCESKEY = new CabalTokenType("js-sources");
  IElementType LDOPTIONSKEY = new CabalTokenType("ld-options");
  IElementType LICENSEFILEKEY = new CabalTokenType("license-file");
  IElementType LICENSEFILESKEY = new CabalTokenType("license-files");
  IElementType LICENSEKEY = new CabalTokenType("license");
  IElementType LOCATIONKEY = new CabalTokenType("location");
  IElementType LT = new CabalTokenType("<");
  IElementType LTEQ = new CabalTokenType("<=");
  IElementType MAINISKEY = new CabalTokenType("main-is");
  IElementType MAINTAINERKEY = new CabalTokenType("maintainer");
  IElementType MANUALKEY = new CabalTokenType("manual");
  IElementType MODULEKEY = new CabalTokenType("module");
  IElementType NAMEKEY = new CabalTokenType("name");
  IElementType NUMBERREGEXP = new CabalTokenType("numberRegexp");
  IElementType OTHEREXTENSIONSKEY = new CabalTokenType("other-extensions");
  IElementType OTHERMODULESKEY = new CabalTokenType("other-modules");
  IElementType PACKAGEKEY = new CabalTokenType("package");
  IElementType PKGCONFIGDEPENDSKEY = new CabalTokenType("pkg-config-depends");
  IElementType SLASH = new CabalTokenType("/");
  IElementType SOURCEREPOSITORYKEY = new CabalTokenType("source-repository");
  IElementType STABILITYKEY = new CabalTokenType("stability");
  IElementType SUBDIRKEY = new CabalTokenType("subdir");
  IElementType SYNOPSISKEY = new CabalTokenType("synopsis");
  IElementType TAGKEY = new CabalTokenType("tag");
  IElementType TESTEDWITHKEY = new CabalTokenType("tested-with");
  IElementType TESTMODULEKEY = new CabalTokenType("test-module");
  IElementType TRUE = new CabalTokenType("true");
  IElementType TYPEKEY = new CabalTokenType("type");
  IElementType VARIDREGEXP = new CabalTokenType("varidRegexp");
  IElementType VERSIONKEY = new CabalTokenType("version");
  IElementType WHITESPACELBRACETOK = new CabalTokenType("Synthetic leftbrace");
  IElementType WHITESPACERBRACETOK = new CabalTokenType("Synthetic rightbrace");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == ADDRESS) {
        return new CabalAddressImpl(node);
      }
      else if (type == AUTHOR) {
        return new CabalAuthorImpl(node);
      }
      else if (type == BENCHMARK) {
        return new CabalBenchmarkImpl(node);
      }
      else if (type == BENCHMARK_KEYS) {
        return new CabalBenchmarkKeysImpl(node);
      }
      else if (type == BOOL) {
        return new CabalBoolImpl(node);
      }
      else if (type == BUG_REPORTS) {
        return new CabalBugReportsImpl(node);
      }
      else if (type == BUILDABLE) {
        return new CabalBuildableImpl(node);
      }
      else if (type == BUILD_DEPENDS) {
        return new CabalBuildDependsImpl(node);
      }
      else if (type == BUILD_INFORMATION) {
        return new CabalBuildInformationImpl(node);
      }
      else if (type == BUILD_TOOLS) {
        return new CabalBuildToolsImpl(node);
      }
      else if (type == BUILD_TYPE) {
        return new CabalBuildTypeImpl(node);
      }
      else if (type == BUILD_TYPE_ENUM) {
        return new CabalBuildTypeEnumImpl(node);
      }
      else if (type == CABAL_PACKAGE) {
        return new CabalCabalPackageImpl(node);
      }
      else if (type == CABAL_VERSION) {
        return new CabalCabalVersionImpl(node);
      }
      else if (type == CATEGORY) {
        return new CabalCategoryImpl(node);
      }
      else if (type == CC_OPTIONS) {
        return new CabalCcOptionsImpl(node);
      }
      else if (type == COMPILER) {
        return new CabalCompilerImpl(node);
      }
      else if (type == CONDITION) {
        return new CabalConditionImpl(node);
      }
      else if (type == CONDITIONAL) {
        return new CabalConditionalImpl(node);
      }
      else if (type == CONDTIONAL_KEY) {
        return new CabalCondtionalKeyImpl(node);
      }
      else if (type == CONFIG) {
        return new CabalConfigImpl(node);
      }
      else if (type == COPYRIGHT) {
        return new CabalCopyrightImpl(node);
      }
      else if (type == CPP_OPTIONS) {
        return new CabalCppOptionsImpl(node);
      }
      else if (type == C_SOURCES) {
        return new CabalCSourcesImpl(node);
      }
      else if (type == DATA_DIR) {
        return new CabalDataDirImpl(node);
      }
      else if (type == DATA_FILES) {
        return new CabalDataFilesImpl(node);
      }
      else if (type == DEFAULT_LANGUAGE) {
        return new CabalDefaultLanguageImpl(node);
      }
      else if (type == DEPENDENCY) {
        return new CabalDependencyImpl(node);
      }
      else if (type == DEPENDENCY_NAME) {
        return new CabalDependencyNameImpl(node);
      }
      else if (type == DESCRIPTION) {
        return new CabalDescriptionImpl(node);
      }
      else if (type == DIRECTORY) {
        return new CabalDirectoryImpl(node);
      }
      else if (type == EXECUTABLE) {
        return new CabalExecutableImpl(node);
      }
      else if (type == EXECUTABLE_KEYS) {
        return new CabalExecutableKeysImpl(node);
      }
      else if (type == EXECUTABLE_SPECIFIC_KEYS) {
        return new CabalExecutableSpecificKeysImpl(node);
      }
      else if (type == EXPOSED) {
        return new CabalExposedImpl(node);
      }
      else if (type == EXTENSIONS) {
        return new CabalExtensionsImpl(node);
      }
      else if (type == EXTRA_DOC_FILES) {
        return new CabalExtraDocFilesImpl(node);
      }
      else if (type == EXTRA_GHCI_LIBRARIES) {
        return new CabalExtraGhciLibrariesImpl(node);
      }
      else if (type == EXTRA_LIBRARIES) {
        return new CabalExtraLibrariesImpl(node);
      }
      else if (type == EXTRA_LIB_DIRS) {
        return new CabalExtraLibDirsImpl(node);
      }
      else if (type == EXTRA_SOURCE_FILES) {
        return new CabalExtraSourceFilesImpl(node);
      }
      else if (type == EXTRA_TMP_FILES) {
        return new CabalExtraTmpFilesImpl(node);
      }
      else if (type == FILE_NAME) {
        return new CabalFileNameImpl(node);
      }
      else if (type == FILE_PATH) {
        return new CabalFilePathImpl(node);
      }
      else if (type == FLAG) {
        return new CabalFlagImpl(node);
      }
      else if (type == FLAG_KEYS) {
        return new CabalFlagKeysImpl(node);
      }
      else if (type == FRAMEWORKS) {
        return new CabalFrameworksImpl(node);
      }
      else if (type == FREEFORM) {
        return new CabalFreeformImpl(node);
      }
      else if (type == GHC_OPTION) {
        return new CabalGhcOptionImpl(node);
      }
      else if (type == GHC_OPTIONS) {
        return new CabalGhcOptionsImpl(node);
      }
      else if (type == GHC_PROF_OPTIONS) {
        return new CabalGhcProfOptionsImpl(node);
      }
      else if (type == GHC_SHARED_OPTIONS) {
        return new CabalGhcSharedOptionsImpl(node);
      }
      else if (type == HOMEPAGE) {
        return new CabalHomepageImpl(node);
      }
      else if (type == HS_SOURCE_DIRS) {
        return new CabalHsSourceDirsImpl(node);
      }
      else if (type == INCLUDES) {
        return new CabalIncludesImpl(node);
      }
      else if (type == INCLUDE_DIRS) {
        return new CabalIncludeDirsImpl(node);
      }
      else if (type == INSTALL_INCLUDES) {
        return new CabalInstallIncludesImpl(node);
      }
      else if (type == JS_SOURCES) {
        return new CabalJsSourcesImpl(node);
      }
      else if (type == KEY) {
        return new CabalKeyImpl(node);
      }
      else if (type == KEY_OR_CONFIG) {
        return new CabalKeyOrConfigImpl(node);
      }
      else if (type == LD_OPTIONS) {
        return new CabalLdOptionsImpl(node);
      }
      else if (type == LIBRARY) {
        return new CabalLibraryImpl(node);
      }
      else if (type == LIBRARY_KEYS) {
        return new CabalLibraryKeysImpl(node);
      }
      else if (type == LIBRARY_SPECIFIC_KEYS) {
        return new CabalLibrarySpecificKeysImpl(node);
      }
      else if (type == LICENSE) {
        return new CabalLicenseImpl(node);
      }
      else if (type == LICENSE_FILE) {
        return new CabalLicenseFileImpl(node);
      }
      else if (type == LICENSE_FILES) {
        return new CabalLicenseFilesImpl(node);
      }
      else if (type == MAINTAINER) {
        return new CabalMaintainerImpl(node);
      }
      else if (type == MAIN_IS) {
        return new CabalMainIsImpl(node);
      }
      else if (type == MODULE) {
        return new CabalModuleImpl(node);
      }
      else if (type == NUMBER) {
        return new CabalNumberImpl(node);
      }
      else if (type == OTHER_EXTENSIONS) {
        return new CabalOtherExtensionsImpl(node);
      }
      else if (type == OTHER_MODULES) {
        return new CabalOtherModulesImpl(node);
      }
      else if (type == PACKAGE_VERSION) {
        return new CabalPackageVersionImpl(node);
      }
      else if (type == PKG_CONFIG_DEPENDS) {
        return new CabalPkgConfigDependsImpl(node);
      }
      else if (type == PROJECT_NAME) {
        return new CabalProjectNameImpl(node);
      }
      else if (type == SOURCE_REPOSITORY) {
        return new CabalSourceRepositoryImpl(node);
      }
      else if (type == SOURCE_REPO_KEYS) {
        return new CabalSourceRepoKeysImpl(node);
      }
      else if (type == STABILITY) {
        return new CabalStabilityImpl(node);
      }
      else if (type == SYNOPSIS) {
        return new CabalSynopsisImpl(node);
      }
      else if (type == TESTED_WITH) {
        return new CabalTestedWithImpl(node);
      }
      else if (type == TEST_INTERFACE) {
        return new CabalTestInterfaceImpl(node);
      }
      else if (type == TEST_MODULES) {
        return new CabalTestModulesImpl(node);
      }
      else if (type == TEST_SUITE) {
        return new CabalTestSuiteImpl(node);
      }
      else if (type == TEST_SUITE_KEYS) {
        return new CabalTestSuiteKeysImpl(node);
      }
      else if (type == TEST_SUITE_SPECIFIC_KEYS) {
        return new CabalTestSuiteSpecificKeysImpl(node);
      }
      else if (type == TEST_SUITE_TYPE) {
        return new CabalTestSuiteTypeImpl(node);
      }
      else if (type == URL) {
        return new CabalUrlImpl(node);
      }
      else if (type == VARID) {
        return new CabalVaridImpl(node);
      }
      else if (type == VERSION) {
        return new CabalVersionImpl(node);
      }
      else if (type == VERSION_CONSTRAINT) {
        return new CabalVersionConstraintImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
