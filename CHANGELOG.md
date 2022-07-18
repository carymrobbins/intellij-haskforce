# HaskForce Changelog

<!-- See: https://github.com/JetBrains/gradle-changelog-plugin -->

## [Unreleased]

- IDEA 2022.1 Support

## [0.3.46]

- IDEA 2021.1 Support (requires Java 11)

## [0.3.45]

- IDEA 2020.3 Support
- Avoid indexing an empty array in HaskellNamesValidator (#439)
- Fix the spellchecker for identifiers with apostrophes
- Module completion from ghc-pkg (optional, experimental)
- Remove Eta support
- Stop displaying 'Type info not found' tooltip

## [0.3.44]

- IDEA 2020.1 Support (#426)
- InsertHoleAsComment intention (#420)
- Detect stack 2 build task dependency source links (#409)
- Parse BlockArguments (#411)
- Bugfixes (#407, #418, #421)

## [0.3.43]

- IntelliJ 2019.2 support (#396)
- Parser improvements

- Optimizations, less recursion, improved recovery
- Parse type decls with class constraints
- Parse DefaultSignatures
- Parse NamedFieldPuns
- Parse 'type' keyword in import


- New intentions

- Add language extensions to package.yaml
- Add dependency to package.yaml
- "Replace with" suggestion
- Fix UnsupportedOperationException (#402)


## [0.3.42]

- Auto-import support via ghc-mod

- Special thanks to @i-am-the-slime for the base implementation!
- Resolve hindent format action bug
- Special thanks to @dplusic for the fix!

- Introducing Stack Task run configuration
- Improvements for ghc-mod integration

- Caching find/browse data
- Lazily spawn after changing settings
- Auto-kill long running, idle ghc-mod processes

- Properly highlight not-in-scope types as red
- Support newer hlint versions (>= 2.1.5) (#365)
- Updated GHC extensions and flags autocompletion
- Ensure module name generated via Create Haskell File action is properly qualified

- Various parser fixes:
- Import consyms (type operators)
- Data families
- DerivingStrategies
- DerivingVia
- DerivingVia
- LambdaCase
- TypeApplications

## [0.3.41]

- Use Scala 2.12 runtime (#348)

## [0.3.40]

- Fix builder classpath (#342)

## [0.3.39]

- Fix indentation parsing (#334)
- Fix errors in HaskellExternalAnnotator (#288)
- Improve spellchecking (#339)
- Added run configs for Etlas (#302)
- Highlight unknown or unused symbols with ghc-mod
- Complete function names in declaration position
- Reload completion cache on user invocation

## [0.3.38] (0.3-beta.38)

- Ensure all output gets processed when building with stack (#325)
- Don't notify for git errors (#324)
- Create Etlas project and builder (#302)

## [0.3.37] (0.3-beta.37)

- Help ghc-mod find stack (#318, #285)
- Exclude .stack-work from sources (#321)
- Don't report typos more than once (#320)
- Parse Eta syntax (#302)

## [0.3.36] (0.3-beta.36)

- Fix Java 6 builder conflict (#278)
- Parse extraneous commas in export/import lists (#284)
- Fix read lock error (#271)
- Strip HTML tags from ghc-modi 'killing' console message (#287)

## [0.3.35] (0.3-beta.35)

- Make HLint aware of extensions from Cabal file (#56)
- Parse qualified operators in patterns (#218)
- Parse 'do rec' syntax (#264)
- Parse pragma-only files (#267)
- Upgraded Grammar-Kit to 1.4.1
- Upgraded JFlex to 1.7.0

## [0.3.34] (0.3-beta.34)

- Fix external builder issue (#282)

## [0.3.33] (0.3-beta.33)

- Require Java 8 (#281)

- The boot JDK for IntelliJ must be running Java 8 (already required for IDEA 2016.1)

- Fixes problems with building Java 6 projects (#278)


- Improved Cabal file support (#169)

- Implemented a proper Cabal parser
- GHC extensions autocompletion
- Jump to source for modules listed in exported-modules
- Determine source roots from Cabal file during Stack project import


- Properly dispose HaskellToolsConsole (#269)

## [0.3.32] (0.3-beta.32)

- Hindent support (#274)
- Fix NPE caused by ghc-modi (#266)

## [0.3.31] (0.3-beta.31)

- Fixed issues with ghc-mod halting the IDE (#259)
- Don't terminate ghc-modi on type info error (#261)
- Allow 'location' values in stack.yaml packages (#263)
- Show working directory in tools console (#251)

## [0.3.30] (0.3-beta.30)

- Fix "expand selection" feature to work with lists (#246)
- Handle missing "packages" field in stack.yaml (#248)
- Validate that project names do not contain whitespace (#252)
- New "Haskell Stack Run" configuration (#252)
- Added Haskell Tools Console (#237)

## [0.3.29] (0.3-beta.29)

- Use `hlint --no-exit-code` to avoid failing on lint warnings (#244)

## [0.3.28] (0.3-beta.28)

- Show build tool config errors in new project dialog
- Parse boolean fields in stack.yaml
- Improved external tool error reporting
- Internal refactors to improve ghc-mod/hlint error highlighting

## [0.3.27] (0.3-beta.27)

- Warn if ghc-mod is compiled with a different version of GHC (#219)
- Ghc-mod now executes in Stack context if applicable (#229)
- Fixed halted IDE when modifying class constraints (#228)
- Fixed importing Stack project at `.` directory (#230)
- Fixed IndexOutOfBounds exception during indexing (#164)

## [0.3.26] (0.3-beta.26)

- Don't override user-specified ghc-modi flags, thanks to @adinapoli (#224)
- Don't attempt to parse directories as stack.yaml (#216, #227)

## [0.3.25] (0.3-beta.25)

- Bug fixes and minor refactoring
- Fix type info errors (#204)
- Fix autocompletion when hiding names (#209)
- Fix ghc-mod parse error messages (#212)

## [0.3.24] (0.3-beta.24)

- Added stack support (#167)
- Added ghc-mod 5.4 support (#182)
- Parse numeric escapes (#196) and haddock comments (#198)

## [0.3.23] (0.3-beta.23)

- Add spellchecker support (#197)
- Fix redundant dashes when splitting comments (#199)

## [0.3.22] (0.3-beta.22)

- Parser improvements: InstanceSigs, MINIMAL pragma for class definitions, and qualified
expressions in
ViewPatterns, relaxed let expressions.

- Autocompletion no longer includes keywords where not applicable.
- Go to definition for exported modules, re-exported functions, and type definitions,
  thanks to @charleso.

## [0.3.21] (0.3-beta.21)

- Type info action only applies to Haskell files (#156)
- ghc-mod completion optimizations
- Added Add Cabal Package and Discover Cabal Packages actions
- Enhanced the new project wizard to configure tools and Cabal package settings
- Resolved hlint annotation issue (#121)
- Honor NoImplicitPrelude pragma for autocompletion

## [0.3.20] (0.3-beta.20)

- Added jump to definition for local variables
- Resolved type info action traceback (#135)
- Updated new project wizard to prompt for SDK (#141)
- Improved operator and comment parsing (#152)

## [0.3.19] (0.3-beta.19)

- Updated ghc-mod(i) integration to use the user-specified GHC and Cabal.
- Highlight mdo and rec as keywords (#120)
- Improved string escape parsing (#111)
- Parse forall in data declarations (#132)
- Parse PACK/UNPACK pragmas for GADTs
- Handle leading slash for QuasiQuote language injections.
- Updated icons for retina display

## [0.3.18] (0.3-beta.18)

- Implemented <b>Type Info</b> action. (#90)
- Implemented language injections for QuasiQuotes.
- Improved import completion with aliased modules.
- Improved <b>Restart ghc-modi</b> action. (#103)
- Implemented creation of subdirectories for create file action, e.g. <b>Foo.Bar.Baz</b>
creates <b>Foo/Bar/Baz.hs</b>
- Fixed hash operator parsing. (#106)
- Improved RankNTypes support. (#92)
- Improved Windows support. (#108, #109)

## [0.3.17] (0.3-beta.17)

- Implemented closed type families.
- Implemented jump to function/variable declarations without type signatures.
- Added module path creation when creating a new file as `Path.To.Module`.

## [0.3.16] (0.3-beta.16)

- Improved ghc-modi integration.

## [0.3.15] (0.3-beta.15)

- Implemented FunctionalDependencies.
- Removed dependency on Apache commons-lang.

## [0.3.14] (0.3-beta.14)

- Implemented RankNTypes for type declarations. (#92)

## [0.3.13] (0.3-beta.13)

- Resolved parser issues with partial case expressions. (#82)

## [0.3.12] (0.3-beta.12)

- Improved "Go to symbol" by indexing declarations.
- Resolved bug with ghc-modi blocking. (#91)
- Prevent ghc-modi from restarting when it terminates unexpectedly.
- Added a "Restart ghc-modi" action to give the user control over when to restart ghc-modi.


## [0.3.11] (0.3-beta.11)

- Fixed duplicate error messages from ghc-modi.
- Improved performance of jump to declaration.
- Added UnicodeSyntax support.
- Added MagicHash syntax support.
- Added tab settings under Code Style settings.
- Run configurations now verify cabal version.

## [0.3.10] (0.3-beta.10)

- Added support for jumping to data and newtype constructor definitions.
- Jump to definition will resolve based on your imports.
- Multi-resolve now displays module names.
- Completion takes into account explicit and hiding imports.
- Reference completion now displays module name instead of file name.

## [0.3.9] (0.3-beta.9)

- Fixed auto-add type signature intention on Windows. (issue #79)
- Fixed ignore hlint intention on Windows. (issue #59)
