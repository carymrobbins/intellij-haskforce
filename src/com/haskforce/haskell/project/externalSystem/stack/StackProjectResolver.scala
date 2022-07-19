package com.haskforce.haskell.project.externalSystem.stack

import java.util.concurrent.{ConcurrentHashMap, ExecutorService, Executors}

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.service.project.ExternalSystemProjectResolver

import scala.util.control.NonFatal

final class StackProjectResolver
  extends ExternalSystemProjectResolver[StackExecutionSettings] {

  import StackProjectResolver._

  private val processMap = new ProcessMap
  private val executorMap = new ExecutorMap

  override def resolveProjectInfo(
    id: ExternalSystemTaskId,
    projectPath: String,
    isPreviewMode: Boolean,
    settings: StackExecutionSettings,
    listener: ExternalSystemTaskNotificationListener
  ): DataNode[ProjectData] = {
    new StackProjectInfoResolver(
      id = id,
      projectPath = projectPath,
      settings = settings,
      listener = listener,
      new WorkManager(id, processMap, executorMap)
    ).resolve()
  }

  override def cancelTask(
    taskId: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = {
    processMap.killAll(taskId)
    executorMap.shutdown(taskId)
    true
  }
}

object StackProjectResolver {

  private val LOG = Logger.getInstance(classOf[StackProjectResolver])

  final class WorkManager(
    taskId: ExternalSystemTaskId,
    processMap: ProcessMap,
    executorMap: ExecutorMap
  ) {

    def proc[A](c: GeneralCommandLine)(f: Process => A): A = {
      val p = c.createProcess()
      processMap.add(taskId, p)
      try {
        val res = f(p)
        if (p.isAlive) p.destroy()
        res
      } finally {
        processMap.remove(taskId, p)
      }
    }

    def compute[A](c: => A): A = {
      executorMap.get(taskId).submit(() => c).get()
    }
  }

  final class ExecutorMap {

    private val internal = new ConcurrentHashMap[ExternalSystemTaskId, ExecutorService]()

    def get(taskId: ExternalSystemTaskId): ExecutorService = {
      internal.compute(
        taskId,
        (_, es) => if (es == null) Executors.newCachedThreadPool() else es
      )
    }

    def shutdown(taskId: ExternalSystemTaskId): Unit = {
      val es = internal.remove(taskId)
      if (es == null) return
      es.shutdownNow()
      ()
    }
  }

  final class ProcessMap {

    private val internal = new ConcurrentHashMap[ExternalSystemTaskId, ProcessSet]()

    def add(taskId: ExternalSystemTaskId, p: Process): Unit = {
      internal.compute(
        taskId,
        (_, s) => {
          val res = if (s == null) new ProcessSet else s
          res.add(p)
          res
        }
      )
      ()
    }

    def remove(taskId: ExternalSystemTaskId, p: Process): Unit = {
      val s = internal.get(taskId)
      if (s == null) return
      s.remove(p)
    }

    def killAll(taskId: ExternalSystemTaskId): Unit = {
      Option(internal.get(taskId)).foreach(_.killAll())
    }
  }

  private final class ProcessSet {

    private val internal = new ConcurrentHashMap[Process, Unit]()

    def add(p: Process): Unit = {
      internal.put(p, ())
    }

    def remove(p: Process): Unit = {
      internal.remove(p)
    }

    def killAll(): Unit = {
      internal.forEach { (p, _) =>
        try {
          p.destroy()
        } catch {
          case NonFatal(e) =>
            LOG.warn(
              s"Exception thrown when cancelling process in StackProjectResolver",
              e
            )
        }
      }
    }
  }
}
