package com.haskforce.actions.cabal

import com.intellij.openapi.module.Module

/**
 * Wrapper class so a JComboBox can hold Modules and display their names appropriately.
 */
sealed case class ComboModuleProxy(module: Module) { override def toString = module.getName }
