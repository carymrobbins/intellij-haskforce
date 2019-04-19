package prelude.imports

trait PreludeImports extends {}
  with prelude.types.TypesImports
  // Custom extension methods
  with prelude.ops.ToEitherOps
  // Scalaz extension methods
  with scalaz.syntax.ToFoldableOps
  with scalaz.syntax.ToMonadOps
  with scalaz.syntax.ToTraverseOps
  with scalaz.syntax.std.ToOptionOps
  with scalaz.syntax.std.ToOptionIdOps
  with scalaz.syntax.ToIdOps
  // Scalaz type class instances
  with scalaz.std.OptionInstances
  with scalaz.std.StreamInstances
  with scalaz.std.VectorInstances
  // Scala/Java converters
  with scala.collection.convert.DecorateAsJava
  with scala.collection.convert.DecorateAsScala
{

  type Monad[F[_]] = scalaz.Monad[F]

  type Traverse[F[_]] = scalaz.Traverse[F]

  type OptionT[F[_], A] = scalaz.OptionT[F, A]
  val OptionT = scalaz.OptionT
}
