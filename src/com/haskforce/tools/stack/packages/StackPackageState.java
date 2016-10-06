package com.haskforce.tools.stack.packages;

import com.haskforce.system.packages.HPackageState;
import com.haskforce.tools.cabal.packages.CabalPackageManager;

import java.io.Serializable;

//Java because we need public fields
@SuppressWarnings("WeakerAccess")
public class StackPackageState implements HPackageState, Serializable {
    private static final long serialVersionUID = 1L;
    public final String packageManager;
    public final String stackPath;

    public StackPackageState(String stackPath) {
        this.stackPath = stackPath;
        packageManager = CabalPackageManager.getName();
    }

    @Override
    public String getPackageManager() {
        return packageManager;
    }
}
