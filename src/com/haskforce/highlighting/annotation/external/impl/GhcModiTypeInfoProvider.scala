package com.haskforce.highlighting.annotation.external
package impl

import com.haskforce.utils.ToEitherObjectOps._
import com.intellij.openapi.editor.LogicalPosition

final class GhcModiTypeInfoProvider(
  ghcModi: GhcModi,
  input: TypeInfoProviderFactory.Input
) extends TypeInfoProvider {

  override def getTypeInfo: TypeInfoProvider.Result = {
    TypeInfoProvider.Result.fromEither(
      Either.nonNull(
        GhcModi.getFuture(
          input.psiFile.getProject,
          ghcModi.`type`(
            input.canonicalFilePath,
            correctFor0BasedVS1Based(input.selectionStart),
            correctFor0BasedVS1Based(input.selectionStop)
          )
        ),
        "ghc-modi failed to obtain type info"
      )
    )
  }

  // TODO: Not sure if this is necessary for LogicalPosition
  private def correctFor0BasedVS1Based(pos0Based: LogicalPosition): LogicalPosition =
    new LogicalPosition(pos0Based.line + 1, pos0Based.column + 1)
}

object GhcModiTypeInfoProviderFactory extends TypeInfoProviderFactory {
  override def get(input: TypeInfoProviderFactory.Input): Option[TypeInfoProvider] =
    GhcModi.get(input.psiFile).map(new GhcModiTypeInfoProvider(_, input))
}
