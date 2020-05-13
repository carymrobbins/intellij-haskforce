package com.haskforce.internal

import com.haskforce.codeInsight.HaskellCompletionCacheLoader
import com.intellij.ide.ApplicationInitializedListener
import com.intellij.openapi.application.{Application, ApplicationManager}
import com.intellij.openapi.project.ProjectManager
import com.intellij.util.messages.Topic

class HaskForceInternal extends ApplicationInitializedListener {
  override def componentsInitialized(): Unit = {
    val app = ApplicationManager.getApplication()
    registerTopic(app, ProjectManager.TOPIC, new HaskellCompletionCacheLoader)
  }

  def registerTopic[A](app: Application, topic: Topic[A], instance: A): Unit = {
    app.getMessageBus.connect(app).subscribe(topic, instance)
  }
}


