package com.haskforce.utils;

import com.intellij.util.Function;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * Helper class to simplify logic patterns.
 */
public class LogicUtil {
    @Nullable
    public static <T, U> List<U> map(final Function<T, U> function, final Collection<T> list) {
        if (list == null) {
            return null;
        }
        ArrayList<U> result = new ArrayList<U>(list.size());
        for (T item : list) {
            result.add(function.fun(item));
        }
        return result;
    }

    @Nullable
    public static <T, U> List<U> map(final Function<T, U> function, final T[] list) {
        if (list == null) {
            return null;
        }
        // Unfortunately, you cannot create a generic array, so we have to use ArrayList.
        ArrayList<U> result = new ArrayList<U>(list.length);
        for (int i = 0; i < list.length; ++i) {
            result.add(i, function.fun(list[i]));
        }
        return result;
    }

    @Nullable
    public static <T> T first(final Function<T, Boolean> predicate, final Collection<T> list) {
        if (list == null) {
            return null;
        }
        for (T x : list) {
            if (predicate.fun(x)) {
                return x;
            }
        }
        return null;
    }
}
