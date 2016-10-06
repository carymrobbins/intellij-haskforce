package com.haskforce.system.packages;

import scala.Option;
import scala.Tuple2;

import java.io.Serializable;

//Java because we need public fields
@SuppressWarnings("WeakerAccess")
public class PersistentStateWrapper implements Serializable {
    private static final long serialVersionUID = 1L;
    public final HPackageState hPackageState;
    public final String location;

    public PersistentStateWrapper(HPackageState hPackageState, String location) {
        this.hPackageState = hPackageState;
        this.location = location;
    }

    @SuppressWarnings("unused")
    public PersistentStateWrapper() {
        hPackageState = null;
        location = null;
    }

    public Option<Tuple2<String, HPackageState>> getState() {
        if (hPackageState == null || location == null) {
            return Option.apply(null);
        }
        return Option.apply(Tuple2.apply(location, hPackageState));
    }
}
