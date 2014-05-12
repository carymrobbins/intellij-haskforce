package com.haskforce.jps.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.ex.JpsCompositeElementBase;
import org.jetbrains.jps.model.ex.JpsElementChildRoleBase;
import org.jetbrains.jps.model.module.JpsModule;

import java.util.Collections;
import java.util.List;

public class JpsHaskellModuleExtension extends JpsCompositeElementBase<JpsHaskellModuleExtension> {
    public static final JpsElementChildRole<JpsHaskellModuleExtension> ROLE = JpsElementChildRoleBase.create("Haskell");

    private final HaskellModuleExtensionProperties myProperties;

    @SuppressWarnings("UnusedDeclaration")
    public JpsHaskellModuleExtension() {
        myProperties = new HaskellModuleExtensionProperties();
    }

    public JpsHaskellModuleExtension(HaskellModuleExtensionProperties properties) {
        myProperties = properties;
    }

    public JpsHaskellModuleExtension(JpsHaskellModuleExtension moduleExtension) {
        myProperties = new HaskellModuleExtensionProperties(moduleExtension.myProperties);
    }

    @NotNull
    @Override
    public JpsHaskellModuleExtension createCopy() {
        return new JpsHaskellModuleExtension(this);
    }

    public HaskellModuleExtensionProperties getProperties() {
        return myProperties;
    }

    public List<String> getParseTransforms() {
        return Collections.unmodifiableList(myProperties.myParseTransforms);
    }

    @Nullable
    public static JpsHaskellModuleExtension getExtension(@Nullable JpsModule module) {
        return module != null ? module.getContainer().getChild(ROLE) : null;
    }
}
