//package com.haskforce.haskell.project.externalSystem.stack
//
//import com.intellij.openapi.externalSystem.model.{Key, ProjectKeys}
//import com.intellij.openapi.externalSystem.model.project.ProjectData
//
//case class StackProjectData(
//  projectName: String,
//  projectPath: String
//) extends
//    ProjectData(
//      StackManager.PROJECT_SYSTEM_ID,
//      projectName,
//      projectPath,
//      projectPath
//    )
//
//object StackProjectData {
//
//  val KEY: Key[StackProjectData] = Key.create(
//    classOf[StackProjectData],
//    ProjectKeys.PROJECT.getProcessingWeight + 1
//  )
//}
