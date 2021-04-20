package chapter15.composableresources

import zio.{ URIO, ZIO }

final case class ZManagedPrimitive[-R, +E, A](
    acquire: ZIO[R, E, A],
    release: A => URIO[R, Any]
) {

  def use[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    acquire.bracket(release)(f)
}
