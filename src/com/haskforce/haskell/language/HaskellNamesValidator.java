package com.haskforce.language;

import com.intellij.lang.refactoring.NamesValidator;
import com.intellij.openapi.project.Project;
import com.intellij.util.containers.HashSet;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

/**
 * Ensures names are legal Haskell. Used by refactoring.
 */
public class HaskellNamesValidator implements NamesValidator {
    /**
     * Slight overapproximation of keywords, but they are keyword in some
     * Haskell variant.
     */
    public static final HashSet<String> HASKELL_KEYWORDS = new HashSet<String>(
            Arrays.asList(new String[]{"as", "case", "class", "data", "default"
                    , "deriving", "do", "else", "export", "forall", "foreign"
                    , "hiding", "if", "import", "in", "infix", "infixl", "infixr"
                    , "instance", "let", "module", "newtype", "of", "qualified"
                    , "safe", "then", "type", "where"}));

    /**
     * Checks if the specified string is a keyword in the language.
     */
    @Override
    public boolean isKeyword(@NotNull String name, Project project) {
        return HASKELL_KEYWORDS.contains(name);
    }

    /**
     * Checks if the specified string is a valid identifier in the language.
     */
    @Override
    public boolean isIdentifier(@NotNull String name, Project project) {
        String[] parts = name.split("\\.");

        for (int i = 0; i < parts.length - 1; i++) {
            String word = parts[i];
            if (!isModid(word, project)) return false;
        }
        return isOneid(parts[parts.length - 1], project, true);
    }

    /**
     * Extended grammar for varid/conid from Haskell 2010 specification. Also allows
     * underscore as start on varids.
     *
     * @param var True for varid, false for conid.
     */
    private boolean isOneid(@NotNull String name, Project project, boolean var) {
        if (name.isEmpty()) return false; // Guards against "Data..Maybe".

        return !isKeyword(name, project);
/*
TODO: Make the name validator context sensitive. A constructor and a variable have different rules.

        if (var) {
            if (!(Character.isLowerCase(name.charAt(0)) || name.startsWith("_"))) return false;
        } else {
            if (!Character.isUpperCase(name.charAt(0))) return false;
        }

        for (char c : name.substring(1).toCharArray()) {
            if (!(Character.isDigit(c) || Character.isUpperCase(c) ||
                    Character.isLowerCase(c) || '\'' == c || '_' == c )) {
                return false;
            }
        }

        // Don't "simplify" this by applying the hint. This logic is readable.
        if (var) {
            return !isKeyword(name, project);
        }
        return true;
*/
    }

    /**
     * Checks whether name is a valid module id.
     */
    private boolean isModid(@NotNull String name, Project project) {
        return isOneid(name, project, false);
    }
}
