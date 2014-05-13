package com.haskforce.jps.model;

import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.serialization.JpsProjectExtensionSerializer;

public class JpsHaskellBuildOptionsSerializer extends JpsProjectExtensionSerializer {
    public static final String HASKELL_BUILD_OPTIONS_COMPONENT_NAME = "HaskellBuildOptions";

    public JpsHaskellBuildOptionsSerializer() {
        super("compiler.xml", HASKELL_BUILD_OPTIONS_COMPONENT_NAME);
    }

    @Override
    public void loadExtension(@NotNull JpsProject project, @NotNull Element componentTag) {
        JpsHaskellBuildOptionsExtension extension = JpsHaskellBuildOptionsExtension.getOrCreateExtension(project);
        HaskellBuildOptions options = XmlSerializer.deserialize(componentTag, HaskellBuildOptions.class);
        if (options != null) {
            extension.setOptions(options);
        }
    }

    @Override
    public void saveExtension(@NotNull JpsProject project, @NotNull Element componentTag) {
    }
}
