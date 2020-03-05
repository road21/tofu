package tofu.syntax

import cats.{Applicative, Functor, Monad}
import cats.syntax.option._
import tofu.Raise
import tofu.syntax.monadic._

object foption {
  def noneF[F[_]: Applicative, A]: F[Option[A]] = none[A].pure[F]

  implicit final class FOptionOps[A](private val a: A) extends AnyVal {
    def someF[F[_]: Applicative]: F[Option[A]] = a.some.pure[F]
  }

  implicit final class FOptionSyntax[F[_], A](private val lhs: F[Option[A]]) extends AnyVal {
    def getOrElseF[B >: A](fa: => F[B])(implicit F: Monad[F]): F[B] =
      lhs.flatMap(_.fold(fa)(F.pure))

    def orElseF(fa: => F[Option[A]])(implicit F: Monad[F]): F[Option[A]] =
      lhs.flatMap {
        case None => fa
        case x    => x.pure[F]
      }

    def orThrow[E](err: => E)(implicit F: Monad[F], FE: Raise[F, E]): F[A] =
      lhs.getOrElseF(FE.raise(err))

    def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): F[Option[B]] =
      lhs.flatMapF(f(_).map(_.some))

    def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): F[Option[B]] =
      lhs.flatMap(_.fold(noneF[F, B])(f(_)))

    def cata[B](default: => B, f: A => B)(implicit F: Functor[F]): F[B] =
      fold(default, f)

    def fold[B](default: => B, f: A => B)(implicit F: Functor[F]): F[B] =
      lhs.map(_.fold(default)(f))

    def toRight[L](l: => L)(implicit F: Functor[F]): F[Either[L, A]] =
      fold(Left(l), Right.apply)

    def toLeft[R](r: => R)(implicit F: Functor[F]): F[Either[A, R]] =
      fold(Right(r), Left.apply)
  }
}
