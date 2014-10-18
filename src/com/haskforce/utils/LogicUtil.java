package com.haskforce.utils;

import com.intellij.util.Function;

import java.util.ArrayList;
import java.util.List;

/**
 * Helper class to simplify logic patterns.
 */
public class LogicUtil {
    public static <T, U> List<U> map(final Function<T, U> function, final List<T> list) {
        if (list == null) {
            return null;
        }
        ArrayList<U> result = new ArrayList<U>(list.size());
        for (int i = 0; i < list.size(); ++i) {
            result.add(i, function.fun(list.get(i)));
        }
        return result;
    }
}
