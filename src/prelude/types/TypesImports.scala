package prelude.types

trait TypesImports {

  type SModule = prelude.types.SModule.Type
  val SModule = prelude.types.SModule

  type SPsiFile[A] = prelude.types.SPsiFile.Type[A]
  val SPsiFile = prelude.types.SPsiFile

  type SPsiElement[A] = prelude.types.SPsiElement.Type[A]
  val SPsiElement = prelude.types.SPsiElement

  type SPsiManager = prelude.types.SPsiManager.Type
  val SPsiManager = prelude.types.SPsiManager

  type SVirtualFile = prelude.types.SVirtualFile.Type
  val SVirtualFile  = prelude.types.SVirtualFile
}
