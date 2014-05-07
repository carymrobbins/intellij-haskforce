package com.haskforce.jps;

import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.ex.JpsElementTypeWithDummyProperties;
import org.jetbrains.jps.model.module.JpsModuleType;

/**
 * Empty shell HaskellModuleType-alike.
 */
public class JpsHaskellModuleType extends JpsElementTypeWithDummyProperties implements JpsModuleType<JpsDummyElement> {
    public static final JpsHaskellModuleType INSTANCE = new JpsHaskellModuleType();

    private JpsHaskellModuleType() {}
}
