package com.haskforce.haskell.project.externalSystem.stack

import java.util

import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.task.ExternalSystemTaskManager

/**
 * Currently, this task manager does nothing.
 *
 * Once we're ready, here's vaguely what we'll need to do.
 *
 * Update [[StackProjectResolver]], and thus, [[StackProjectInfoResolver]],
 * to construct [[com.intellij.openapi.externalSystem.model.task.TaskData]]
 * nodes and insert them into the built tree.
 *
 * I think these tasks should appear in the build tool window (which as of this
 * writing does not yet exist). When developing HaskForce, you should see a
 * 'Gradle' tool window available, usually on the right panel. We would
 * likely need to build this out as well for 'Stack'.
 *
 * We'll need to create an subclass of
 * [[com.intellij.openapi.externalSystem.service.execution.ExternalSystemRunConfiguration]].
 * See [[org.jetbrains.plugins.gradle.service.execution.GradleRunConfiguration]]
 * as an example.
 *
 * There's probably more involved that I'm not yet aware of, so good luck.
 */
final class StackTaskManager
  extends ExternalSystemTaskManager[StackExecutionSettings] {

  override def executeTasks(
    id: ExternalSystemTaskId,
    taskNames: util.List[String],
    projectPath: String,
    settings: StackExecutionSettings,
    jvmAgentSetup: String,
    listener: ExternalSystemTaskNotificationListener
  ): Unit = {
  }

  override def cancelTask(
    id: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = {
    false
  }
}
