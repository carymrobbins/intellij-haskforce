package com.haskforce.tools.cabal.packages;

import com.haskforce.system.packages.HPackageState;

import java.io.Serializable;

//Java because we need public fields
@SuppressWarnings("WeakerAccess")
public class CabalPackageState implements HPackageState, Serializable {
    private static final long serialVersionUID = 1L;
    public final String packageManager;

    public CabalPackageState() {
        packageManager = CabalPackageManager.getName();
    }

    @Override
    public String getPackageManager() {
        return packageManager;
    }
}
