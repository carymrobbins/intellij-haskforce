package com.haskforce.highlighting;

import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl;
import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.TextRange;
import com.intellij.xml.util.XmlStringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Pattern;

public abstract class HaskellExternalAnnotatorBase<InitialInfoType, AnnotationResultType> extends ExternalAnnotator<InitialInfoType, AnnotationResultType> {
    private static final Logger LOG = Logger.getInstance(HaskellExternalAnnotatorBase.class);
    private static final Pattern NEWLINE_REGEX = Pattern.compile("\n");
    private static final Pattern SPACE_REGEX = Pattern.compile(" ");

    public static String escapeSpacesForHtml(String string) {
        return string == null ? null : SPACE_REGEX.matcher(NEWLINE_REGEX.matcher(string).replaceAll("<br/>")).replaceAll("&nbsp;");
    }

    // Customize the annotation methods to allow HTML in annotations.
    public static Annotation createAnnotation(@NotNull HighlightSeverity severity, @NotNull AnnotationHolder holder, @NotNull TextRange range, @Nullable String message) {
        if (holder instanceof AnnotationHolderImpl) {
            String tooltip = message == null ? null : XmlStringUtil.wrapInHtml(escapeSpacesForHtml(XmlStringUtil.escapeString(message)));
            Annotation annotation = new Annotation(range.getStartOffset(), range.getEndOffset(), severity, message, tooltip);
            ((AnnotationHolderImpl)holder).add(annotation);
            return annotation;
        }
        // AnnotationHolder.createAnnotation() doesn't exist in IntelliJ 13.0
        if (severity == HighlightSeverity.INFORMATION) {
            return holder.createInfoAnnotation(range, message);
        }
        if (severity == HighlightSeverity.WEAK_WARNING) {
            return holder.createWeakWarningAnnotation(range, message);
        }
        if (severity == HighlightSeverity.WARNING) {
            return holder.createWarningAnnotation(range, message);
        }
        if (severity == HighlightSeverity.ERROR) {
            return holder.createErrorAnnotation(range, message);
        }
        if (severity == HighlightSeverity.GENERIC_SERVER_ERROR_OR_WARNING) {
            return holder.createErrorAnnotation(range, message);
        }
        LOG.warn("Unknown severity: " + severity);
        return holder.createInfoAnnotation(range, message);
    }

    public static Annotation createInfoAnnotation(@NotNull AnnotationHolder holder, @NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.INFORMATION, holder, range, message);
    }

    public static Annotation createWeakWarningAnnotation(@NotNull AnnotationHolder holder, @NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.WEAK_WARNING, holder, range, message);
    }

    public static Annotation createWarningAnnotation(@NotNull AnnotationHolder holder, @NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.WARNING, holder, range, message);
    }

    public static Annotation createErrorAnnotation(@NotNull AnnotationHolder holder, @NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.ERROR, holder, range, message);
    }
}
