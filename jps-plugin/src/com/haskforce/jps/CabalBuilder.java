/*
 * Copyright 2000-2012 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.haskforce.jps;

/*
 * Downloaded from https://github.com/Atsky/haskell-idea-plugin on 7 May
 * 2014.
 */

import com.haskforce.jps.model.HaskellBuildOptions;
import com.haskforce.jps.model.JpsHaskellBuildOptionsExtension;
import com.haskforce.jps.model.JpsHaskellModuleType;
import com.haskforce.system.utils.SystemUtil;
import com.intellij.execution.ExecutionException;
import org.jetbrains.jps.incremental.BuilderCategory;
import org.jetbrains.jps.incremental.ModuleLevelBuilder;
import com.intellij.openapi.diagnostic.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.ModuleChunk;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.builders.java.JavaSourceRootDescriptor;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ModuleBuildTarget;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.incremental.messages.ProgressMessage;
import org.jetbrains.jps.model.module.JpsModule;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * First stop that does any work in the Cabal builder.
 */
public class CabalBuilder extends ModuleLevelBuilder {
    // Messages go to the log available in Help -> Show log in finder,
    // "build-log" subdirectory.
    private final static Logger LOG = Logger.getInstance(CabalBuilder.class);

    public CabalBuilder() {
        super(BuilderCategory.TRANSLATOR);
    }

    /**
     * Build the project including all modules with Cabal.
     */
    public ExitCode build(final CompileContext context,
                          final ModuleChunk chunk,
                          final DirtyFilesHolder<JavaSourceRootDescriptor, ModuleBuildTarget> dirtyFilesHolder,
                          final OutputConsumer outputConsumer) throws ProjectBuildException {
        try {
            for (JpsModule module : chunk.getModules()) {
                // Builder gets called for pure java projects as well. Silently
                // skip those since processMessage() of error will halt the
                // entire compilation. Funny..
                if (!module.getModuleType().equals(JpsHaskellModuleType.INSTANCE)) {
                    continue;
                }

                HaskellBuildOptions buildOptions = JpsHaskellBuildOptionsExtension.getOrCreateExtension(module.getProject()).getOptions();
                // If we're not using Cabal, don't use Cabal.
                if (!buildOptions.myUseCabal || buildOptions.myUseStack) {
                    continue;
                }

                File cabalFile = getCabalFile(module);
                if (cabalFile == null) {
                    context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.ERROR,
                            "Can not find cabal file in " + getContentRootPath(module)));
                    continue;
                }
                //noinspection ObjectAllocationInLoop
                CabalJspInterface cabal = new CabalJspInterface(cabalFile, buildOptions);

                //noinspection ObjectAllocationInLoop
                if (buildOptions.myUseCabalSandbox) {
                    if (runSandboxInit(context, module, cabal, cabalFile)) return ExitCode.ABORT;
                }
                if (buildOptions.myInstallCabalDependencies) {
                    if (runInstallDependencies(context, module, cabal)) return ExitCode.ABORT;
                }
                if (runConfigure(context, module, cabal)) return ExitCode.ABORT;
                if (runBuild(context, module, cabal)) return ExitCode.ABORT;
            }
            return ExitCode.OK;
        } catch (IOException e) {
            processCabalError(context, e);
        } catch (InterruptedException e) {
            processCabalError(context, e);
        } catch (ExecutionException e) {
            processCabalError(context, e);
        }
        return ExitCode.ABORT;
    }

    private static void processCabalError(final CompileContext context, final Exception e) {
        e.printStackTrace();
        context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.ERROR, e.getMessage()));
    }

    /**
     * Runs cabal build.
     */
    private static boolean runBuild(CompileContext context, JpsModule module, CabalJspInterface cabal)
            throws IOException, InterruptedException, ExecutionException {
        context.processMessage(new ProgressMessage("cabal build"));
        context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.INFO, "Start build"));
        Process buildProcess = cabal.build();
        processOut(context, buildProcess, module);

        if (buildProcess.waitFor() != 0) {
            context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.ERROR, "build errors."));
            return true;
        }
        return false;
    }

    /**
     * Runs cabal configure.
     */
    private static boolean runConfigure(CompileContext context, JpsModule module, CabalJspInterface cabal)
            throws IOException, InterruptedException, ExecutionException {
        context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.INFO, "Start configure"));

        Process configureProcess = cabal.configure();

        processOut(context, configureProcess, module);

        if (configureProcess.waitFor() != 0) {
            context.processMessage(new CompilerMessage(
                    "cabal",
                    BuildMessage.Kind.ERROR,
                    "configure failed."));
            return true;
        }
        return false;
    }

    /**
     * Runs cabal install --only-dependencies
     */
    private static boolean runInstallDependencies(CompileContext context, JpsModule module, CabalJspInterface cabal)
            throws IOException, InterruptedException, ExecutionException {

        if (!installDependenciesRequired(cabal)) {
            return false;
        }

        context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.INFO, "Install dependencies"));

        Process installDependenciesProcess = cabal.installDependencies();

        processOut(context, installDependenciesProcess, module, true);

        if (installDependenciesProcess.waitFor() != 0) {
            context.processMessage(new CompilerMessage(
                    "cabal",
                    BuildMessage.Kind.ERROR,
                    "install dependencies failed."));
            return true;
        }
        return false;
    }

    private static boolean installDependenciesRequired(CabalJspInterface cabal) throws IOException, ExecutionException {
        Process dryRun = cabal.installDependencies(true);
        Iterator<String> dryRunOut = collectOutput(dryRun);
        String line;
        while (dryRunOut.hasNext()) {
            line = dryRunOut.next();
            // TODO: Is there a more elegant way to do this?
            if (line.contains("already installed")) {
                return false;
            }
        }
        return true;
    }

    /**
     * Runs cabal sandbox init
     */
    private static boolean runSandboxInit(CompileContext context, JpsModule module, CabalJspInterface cabal, File cabalFile)
            throws IOException, InterruptedException, ExecutionException {

        // If sandbox already exists, no need to run init - just bail out.
        if (new File(cabalFile.getParent(), "cabal.sandbox.config").isFile()) {
            return false;
        }

        context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.INFO, "Create sandbox"));

        Process sandboxProcess = cabal.sandboxInit();

        if (sandboxProcess.waitFor() != 0) {
            context.processMessage(new CompilerMessage(
                    "cabal",
                    BuildMessage.Kind.ERROR,
                    "sandbox init failed."));
            return true;
        }
        return false;
    }

    /**
     * Parses output from cabal and signals errors/warnings to the IDE.
     */
    private static void processOut(CompileContext context, Process process, JpsModule module) {
        processOut(context, process, module, false);
    }

    private static void processOut(CompileContext context, Process process, JpsModule module, boolean logAll) {
        final String warningPrefix = "Warning: ";
        final String cabalPrefix = "cabal: ";
        boolean oneBehind = false;
        String line = "";
        Iterator<String> processOut = collectOutput(process);
        StringBuilder msg = new StringBuilder(1000);
        Pattern compiledPattern = Pattern.compile("^([^:]+):(\\d+):(\\d+:)?(.*$)?",
                                                Pattern.MULTILINE);
        Pattern progressPattern = Pattern.compile("^\\[(\\d+) of (\\d+)\\]");

        while (processOut.hasNext() || oneBehind) {
            if (oneBehind) {
                oneBehind = false;
            } else {
                line = processOut.next();
            }

            // See comment after this method for example warning message.
            Matcher matcher = compiledPattern.matcher(line);
            Matcher progressMatcher = progressPattern.matcher(line);
            if (line.startsWith(warningPrefix)) {
                // Cabal warnings.
                String text = line.substring(warningPrefix.length()) + SystemUtil.LINE_SEPARATOR + processOut.next();
                //noinspection ObjectAllocationInLoop
                context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.WARNING, text));
            } else if (line.startsWith(cabalPrefix)) {
                // Unknown cabal messages. Exit code will tell if they were
                // errors. Just forward to user.
                String text = line.substring(cabalPrefix.length()) + SystemUtil.LINE_SEPARATOR + processOut.next();
                //noinspection ObjectAllocationInLoop
                context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.WARNING, text));
            } else if (matcher.find()) {
                // GHC Messages
                String file = matcher.group(1);
                long lineNum = Long.parseLong(matcher.group(2));
                // We might not have a column, but if we do it ends with a colon.
                // Make sure to filter that colon out.
                long colNum = matcher.group(3) == null ? 0 :
                        Long.parseLong(matcher.group(3).substring(0, matcher.group(3).length() - 1));
                msg.setLength(0);
                msg.append(matcher.group(4));
                while (processOut.hasNext()) {
                    line = processOut.next();

                    if (line.endsWith("warning generated.") ||
                            line.trim().length() == 0) {
                        break;
                    }
                    if (line.startsWith("[") || line.startsWith("In-place")) {
                        // Fresh line starting, save to process next.
                        oneBehind = true;
                        break;
                    }
                    msg.append(line).append(SystemUtil.LINE_SEPARATOR);
                }

                // RootPath necessary for reasonable error messages by Intellij.
                String sourcePath = getContentRootPath(module) + File.separator + file.replace('\\', File.separatorChar);
                // GHC emits warning messages that look like:
                // File.hs:line:column: Warning..
                // Messages that do not start with "Warning" are errors. Other
                // tools (happy for example) do not emit columns and messages
                // not starting with "Warning" can still be warnings. Assume
                // tools without column information warn.
                //
                // If our assumption is wrong that will be corrected by the
                // return value from cabal build. If we assume the opposite we
                // will halt working builds with a (faulty) error.
                BuildMessage.Kind kind = matcher.group(4).trim().startsWith("Warning") ||
                        matcher.group(3) == null ? BuildMessage.Kind.WARNING : BuildMessage.Kind.ERROR;

                final String trimmedMessage = msg.toString().trim();
                if (trimmedMessage.isEmpty()) { // DEBUG code for #31.
                    //noinspection ObjectAllocationInLoop
                    context.processMessage(new CompilerMessage(
                            "ghc",
                            BuildMessage.Kind.WARNING,
                            "INTERNAL HaskForce ERROR: Got an empty compiler message from:\n" + msg + "\nWith line:\n" + line + '\n',
                            sourcePath,
                            -1L, -1L, -1L,
                            lineNum, colNum));
                } else {
                    //noinspection ObjectAllocationInLoop
                    context.processMessage(new CompilerMessage(
                            "ghc",
                            kind,
                            trimmedMessage,
                            sourcePath,
                            -1L, -1L, -1L,
                            lineNum, colNum));
                }
            } else if (logAll) {
                //noinspection ObjectAllocationInLoop
                context.processMessage(new CompilerMessage("cabal", BuildMessage.Kind.INFO, processOut.next()));
            } else if (progressMatcher.find()) {
                // Update progress tracker. Seems pretty coarse-grained.
                int fileNum = Integer.parseInt(progressMatcher.group(1));
                int totalFiles = Integer.parseInt(progressMatcher.group(2));
                if (totalFiles != 0) {
                    context.setDone((float) fileNum / totalFiles);
                }
            }
            if (context.getCancelStatus().isCanceled()) {
                LOG.info("Build cancelled, terminating..");
                process.destroy();
                break;
            }
        }
    }
    /*

Path warning:

cabal: The program 'happy' version >=1.17 is required but it could not be found

Example warning:

Preprocessing library feldspar-language-0.6.1.0...
[74 of 92] Compiling Feldspar.Core.UntypedRepresentation ( src/Feldspar/Core/UntypedRepresentation.hs, dist/build/Feldspar/Core/UntypedRepresentation.o )
src/Feldspar/Core/UntypedRepresentation.hs:483:5: Warning:
    Pattern match(es) are overlapped
    In an equation for `typeof': typeof e = ...
[74 of 92] Compiling Feldspar.Core.UntypedRepresentation ( src/Feldspar/Core/UntypedRepresentation.hs, dist/build/Feldspar/Core/UntypedRepresentation.p_o )
<same warning again>

Example errors:

src/Main.hs:14:3:
    Could not deduce (ToJSON (ModulePragma l0))
      arising from a use of `toJSON'
    from the context (ToJSON l0)
      bound by the instance declaration at src/Main.hs:14:3-38
    Possible fix:
      add an instance declaration for (ToJSON (ModulePragma l0))
    In the third argument of `vector-0.10.9.1:Data.Vector.Mutable.unsafeWrite', namely
      `toJSON arg3_a4Di'
    In a stmt of a 'do' block:
      vector-0.10.9.1:Data.Vector.Mutable.unsafeWrite
        mv_a4Dn 2 (toJSON arg3_a4Di)
    In the first argument of `vector-0.10.9.1:Data.Vector.create', namely
      `do { mv_a4Dn <- vector-0.10.9.1:Data.Vector.Mutable.unsafeNew 7;
            vector-0.10.9.1:Data.Vector.Mutable.unsafeWrite
              mv_a4Dn 0 (toJSON arg1_a4Dg);
            vector-0.10.9.1:Data.Vector.Mutable.unsafeWrite
              mv_a4Dn 1 (toJSON arg2_a4Dh);
            vector-0.10.9.1:Data.Vector.Mutable.unsafeWrite
              mv_a4Dn 2 (toJSON arg3_a4Di);
            .... }'

src/Main.hs..

Happy error:

Preprocessing library haskell-src-exts-1.15.0.1...
src/Language/Haskell/Exts/InternalParser.ly:962: Parse error

Nothing to do error:

Peters-MacBook-Air-472:haskell-src-exts pj$ cabal build -j5
Building haskell-src-exts-1.15.0.1...
Preprocessing library haskell-src-exts-1.15.0.1...
In-place registering haskell-src-exts-1.15.0.1...
Preprocessing test suite 'test' for haskell-src-exts-1.15.0.1...
Peters-MacBook-Air-472:haskell-src-exts pj$
     */

    private static Iterator<String> collectOutput(Process process) {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        return new Iterator<String>() {

            String line = null;

            @Override
            public boolean hasNext() {
                return fetch() != null;
            }

            private String fetch() {
                if (line == null) {
                    try {
                        line = reader.readLine();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                return line;
            }

            @Override
            public String next() throws NoSuchElementException {
                String result = fetch();
                line = null;
                return result;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    /**
     * Searches for the cabal file in the module top directory.
     */
    private static File getCabalFile(JpsModule module) {
        String pathname = getContentRootPath(module);
        //noinspection ConstantConditions
        for (File file : new File(pathname).listFiles()) {
            if (file.getName().endsWith(".cabal")) {
                return file;
            }
        }
        return null;
    }

    private static String getContentRootPath(JpsModule module) {
        String url = module.getContentRootsList().getUrls().get(0);
        return url.substring("file://".length());
    }

    /**
     * Reports that we can compile hs and lhs files.
     */
    @Override
    public List<String> getCompilableFileExtensions() {
        return Arrays.asList("hs", "lhs");
    }

    @Override
    public String toString() {
        return getPresentableName();
    }

    @NotNull
    public String getPresentableName() {
        return "Cabal builder";
    }
}
