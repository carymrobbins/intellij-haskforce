package com.haskforce.highlighting.annotation.external

/** A component which provides type info for a  */
trait TypeInfoProvider {
  def getTypeInfo: TypeInfoProvider.Result
}

object TypeInfoProvider {

  type Result = Either[Failure, Success]

  object Result {
    def failure(message: String): Result = Left(Failure(message))
    def success(typeInfo: String): Result = Right(Success(typeInfo))

    def fromEither(x: Either[String, String]): Result = x match {
      case Left(s) => failure(s)
      case Right(s) => success(s)
    }

    def fromOption(x: Option[String], message: String): Result = x match {
      case None => failure(message)
      case Some(s) => success(s)
    }
  }

  final case class Failure(message: String)
  final case class Success(typeInfo: String)
}
