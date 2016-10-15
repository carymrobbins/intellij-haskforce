package com.haskforce.system.packages;

import com.intellij.util.xmlb.annotations.Attribute;
import scala.Option;
import scala.Tuple2;

import java.io.Serializable;

//Java because we need public fields
@SuppressWarnings("WeakerAccess")
public class HPackageModuleState implements Serializable {
    private static final long serialVersionUID = 1L;
    @Attribute
    public String hPackageState;
    @Attribute
    public String location;
    @Attribute
    private String packageManager;

    public HPackageModuleState(String hPackageState, String packageManager, String location) {
        this.hPackageState = hPackageState;
        this.location = location;
        this.packageManager = packageManager;
    }

    @SuppressWarnings("unused")
    public HPackageModuleState() {
        hPackageState = null;
        location = null;
    }

    public Option<Tuple2<String, HPackageState>> getState() {
        if (packageManager == null || location == null) {
            return Option.apply(null);
        }
        return Option.apply(Tuple2.apply(location, HPackageState.apply(hPackageState, packageManager)));
    }
}
