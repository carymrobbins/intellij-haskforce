package com.haskforce.jps.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.ex.JpsCompositeElementBase;
import org.jetbrains.jps.model.ex.JpsElementChildRoleBase;

public class JpsHaskellBuildOptionsExtension extends JpsCompositeElementBase<JpsHaskellBuildOptionsExtension> {
    public static final JpsElementChildRole<JpsHaskellBuildOptionsExtension> ROLE = JpsElementChildRoleBase.create("HaskellBuildOptions");

    private HaskellBuildOptions myOptions;

    public JpsHaskellBuildOptionsExtension(HaskellBuildOptions options) {
        myOptions = options;
    }

    @NotNull
    @Override
    public JpsHaskellBuildOptionsExtension createCopy() {
        return new JpsHaskellBuildOptionsExtension(new HaskellBuildOptions(myOptions));
    }

    public HaskellBuildOptions getOptions() {
        return myOptions;
    }

    public void setOptions(HaskellBuildOptions options) {
        myOptions = options;
    }

    @NotNull
    public static JpsHaskellBuildOptionsExtension getOrCreateExtension(@NotNull JpsProject project) {
        JpsHaskellBuildOptionsExtension extension = project.getContainer().getChild(ROLE);
        if (extension == null) {
            extension = project.getContainer().setChild(ROLE, new JpsHaskellBuildOptionsExtension(new HaskellBuildOptions()));
        }
        return extension;
    }

    @Override
    public String toString() {
        return "JpsHaskellBuildOptionsExtension{" +
                "myOptions=" + myOptions +
                '}';
    }
}
