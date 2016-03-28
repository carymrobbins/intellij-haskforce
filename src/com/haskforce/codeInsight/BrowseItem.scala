package com.haskforce.codeInsight

import com.intellij.codeInsight.lookup.LookupElement

case class BrowseItem(name: String, module: String, typ: String) {
  def toLookupElement: LookupElement = LookupElementUtil.fromBrowseItem(this)
}
