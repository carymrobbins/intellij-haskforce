package com.haskforce.settings;

public class ToolSettings {
    private String path;
    private String flags;

    public String getPath() {
        return path;
    }

    public String getFlags() {
        return flags;
    }

    public ToolSettings(String path, String flags) {
        this.path = path;
        this.flags = flags;
    }
}
