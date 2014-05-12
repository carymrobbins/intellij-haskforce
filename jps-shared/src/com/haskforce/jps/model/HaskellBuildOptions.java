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
        myAddDebugInfoEnabled = options.myAddDebugInfoEnabled;
    }

    @Tag("useCabal")
    public boolean myUseCabal = true;

    @Tag("useCabalSandbox")
    public boolean myUseCabalSandbox = false;


    @Tag("useDebugInfo")
    public boolean myAddDebugInfoEnabled = true;
}
