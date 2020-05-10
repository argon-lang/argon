package dev.argon.compiler.types

final case class SubTypeInfo[+T]
(
  superType: T,
  subType: T,
  args: Vector[SubTypeInfo[T]],
) {

  def map[T2 >: T, U](f: T2 => U): SubTypeInfo[U] =
    SubTypeInfo(
      f(superType),
      f(subType),
      args.map { _.map(f) }
    )

}

