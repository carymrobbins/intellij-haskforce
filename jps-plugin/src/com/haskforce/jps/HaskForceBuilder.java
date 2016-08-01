package com.haskforce.jps;

import com.haskforce.jps.stack.StackBuilder;
import com.haskforce.utils.JavaVersionUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.incremental.BuilderService;
import org.jetbrains.jps.incremental.ModuleLevelBuilder;
import org.jetbrains.jps.incremental.TargetBuilder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Main entry for the external builder interface.
 */
public class HaskForceBuilder extends BuilderService {
    @NotNull
    @Override
    public List<? extends ModuleLevelBuilder> createModuleLevelBuilders() {
        // See: https://github.com/carymrobbins/intellij-haskforce/issues/278
        // When we are compiling a Haskell project, the JDK should be at least Java 8.
        // Only Java projects will use something older.
        if (!JavaVersionUtil.isAtLeastJava8()) return Collections.emptyList();
        return Collections.singletonList(new CabalBuilder());
    }

    @NotNull
    @Override
    public List<? extends TargetBuilder<?, ?>> createBuilders() {
        return Collections.singletonList(new StackBuilder());
    }

    @NotNull
    @Override
    public List<HaskellTargetType> getTargetTypes() {
        return Arrays.asList(HaskellTargetType.PRODUCTION, HaskellTargetType.TESTS);
    }
}
