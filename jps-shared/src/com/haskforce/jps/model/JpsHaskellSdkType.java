package com.haskforce.jps.model;

import com.intellij.openapi.util.SystemInfo;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsElementFactory;
import org.jetbrains.jps.model.JpsElementTypeWithDefaultProperties;
import org.jetbrains.jps.model.library.sdk.JpsSdkType;

import java.io.File;

public class JpsHaskellSdkType extends JpsSdkType<JpsDummyElement> implements JpsElementTypeWithDefaultProperties<JpsDummyElement> {
    public static final JpsHaskellSdkType INSTANCE = new JpsHaskellSdkType();

    @NotNull
    public static File getExecutable(@NotNull final String path, @NotNull final String command) {
        return new File(path, SystemInfo.isWindows ? command + ".exe" : command);
    }

    @NotNull
    public static File getByteCodeCompilerExecutable(@NotNull final String sdkHome) {
        return getExecutable(new File(sdkHome, "bin").getAbsolutePath(), "ghc");
    }

    @NotNull
    @Override
    public JpsDummyElement createDefaultProperties() {
        return JpsElementFactory.getInstance().createDummyElement();
    }
}
