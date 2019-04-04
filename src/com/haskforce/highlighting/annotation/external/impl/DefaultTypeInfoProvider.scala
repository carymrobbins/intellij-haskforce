package com.haskforce.highlighting.annotation.external
package impl

import com.haskforce.highlighting.annotation.external.TypeInfoProvider.Result
import scala.collection.mutable

final class DefaultTypeInfoProvider(
  input: TypeInfoProviderFactory.Input
) extends TypeInfoProvider {

  override def getTypeInfo: Result = {
    val failureBuffer = new mutable.ArrayBuffer[String]()
    DefaultTypeInfoProvider.factories.foreach { factory =>
      factory.get(input).foreach { provider =>
        provider.getTypeInfo match {
          case r@Right(_) => return r
          case Left(failure) => failureBuffer += failure.message
        }
      }
    }
    TypeInfoProvider.Result.failure(
      "All configured type info providers failed to obtain type info; " +
      "failures were: " + failureBuffer.mkString(";")
    )
  }
}

object DefaultTypeInfoProvider {

  def get(input: TypeInfoProviderFactory.Input): TypeInfoProvider =
    new DefaultTypeInfoProvider(input)

  // Should be in order of priority. On failure, continue to the next
  // factory. On success, return the result.
  private val factories: List[TypeInfoProviderFactory] = List(
    PsiTypeInferenceTypeInfoProviderFactory,
    GhcModiTypeInfoProviderFactory
  )
}
