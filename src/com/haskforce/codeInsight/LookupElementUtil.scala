package com.haskforce.codeInsight

import com.haskforce.HaskellIcons
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.util.containers.ContainerUtil
import com.haskforce.Implicits._

object LookupElementUtil {
  def create(name: String, module: String, typ: String): LookupElement = {
    LookupElementBuilder.create(name).withIcon(HaskellIcons.FILE)
      .withTailText(s" ($module)", true)
      .withTypeText(typ)
  }

  def fromString(s: String): LookupElement = {
    LookupElementBuilder.create(s).withIcon(HaskellIcons.FILE)
  }

  def fromStrings(ss: Array[String]): java.util.List[LookupElement] = {
    java.util.Arrays.asList(ss.map(fromString): _*)
  }

  def fromStrings(ss: java.util.Collection[String]): java.util.List[LookupElement] = {
    ContainerUtil.map[String, LookupElement](ss, fromString _)
  }

  def fromBrowseItem(b: BrowseItem): LookupElement = {
    create(b.name, b.module, b.typ)
  }
}
