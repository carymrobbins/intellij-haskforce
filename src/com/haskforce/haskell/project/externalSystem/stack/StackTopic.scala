package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.util.messages.Topic

object StackTopic extends Topic[StackProjectSettingsListener](
  "Stack project settings", classOf[StackProjectSettingsListener]
) {
  // Useful for calling from Java.
  def get(): StackTopic.type = this
}
