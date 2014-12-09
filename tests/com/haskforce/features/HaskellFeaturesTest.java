package com.haskforce.features;

import com.haskforce.HaskellFileType;
import com.haskforce.utils.SystemUtil;
import com.intellij.codeInsight.generation.actions.CommentByBlockCommentAction;
import com.intellij.codeInsight.generation.actions.CommentByLineCommentAction;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;

/**
 * Features test driver. Add new feature testcases here.
 */
public class HaskellFeaturesTest extends LightPlatformCodeInsightFixtureTestCase {
    public HaskellFeaturesTest() {
        super();
    }

    public void testCommenter00001() {
        myFixture.configureByText(HaskellFileType.INSTANCE, "<caret>f acc [] = reverse acc");
        CommentByLineCommentAction commentAction = new CommentByLineCommentAction();
        commentAction.actionPerformedImpl(getProject(), myFixture.getEditor());
        myFixture.checkResult("-- f acc [] = reverse acc");
        commentAction.actionPerformedImpl(getProject(), myFixture.getEditor());
        myFixture.checkResult("f acc [] = reverse acc");
    }

    public void testCommenter00002() {
        myFixture.configureByText(HaskellFileType.INSTANCE, "f acc [] = acc" +
                SystemUtil.LINE_SEPARATOR + "<selection>f acc (x:xs) = f (x:acc) xs</selection>" +
                SystemUtil.LINE_SEPARATOR + "<caret>f _ _ = error \"impossible!\"");
        CommentByBlockCommentAction commentAction = new CommentByBlockCommentAction();
        commentAction.actionPerformedImpl(getProject(), myFixture.getEditor());
        myFixture.checkResult("f acc [] = acc" + SystemUtil.LINE_SEPARATOR +
                "{-f acc (x:xs) = f (x:acc) xs-}" + SystemUtil.LINE_SEPARATOR +
                "f _ _ = error \"impossible!\"");
    }
}
