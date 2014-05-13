package com.haskforce.jps.model;

import com.intellij.util.xmlb.annotations.Tag;

/**
 * Serialization object for communicating build settings with the build server.
 */
public class HaskellBuildOptions {
    public HaskellBuildOptions() {
    }

    public HaskellBuildOptions(HaskellBuildOptions options) {
        myUseCabal = options.myUseCabal;
        myUseCabalSandbox = options.myUseCabalSandbox;
        myProfilingBuild = options.myProfilingBuild;
        myGhcPath = options.myGhcPath;
        myCabalPath = options.myCabalPath;
    }

    @Tag("useCabal")
    public boolean myUseCabal = true;

    @Tag("useCabalSandbox")
    public boolean myUseCabalSandbox = false;

    @Tag("useProfilingBuild")
    public boolean myProfilingBuild = true;

    @Tag("ghcPath")
    public String myGhcPath = "ghc";

    @Tag("cabalPath")
    public String myCabalPath = "cabal";

    @Override
    public String toString() {
        return "HaskellBuildOptions{" +
                "myUseCabal=" + myUseCabal +
                ", myUseCabalSandbox=" + myUseCabalSandbox +
                ", myProfilingBuild=" + myProfilingBuild +
                ", myGhcPath=" + myGhcPath +
                ", myCabalPath=" + myCabalPath +
                '}';
    }
}
