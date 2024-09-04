package nyub.build_systems_a_la_carte.monads

trait Functor[F[_]]:
    extension [A](fa: F[A])
        def map[B](f: A => B): F[B]
        final infix def `<&>`[B](f: A => B): F[B] = fa.map(f)

    extension [A, B](f: A => B) final infix def `<$>`(fa: F[A]): F[B] = fa.map(f)

end Functor

trait Applicative[F[_]] extends Functor[F]:
    def pure[A](a: => A): F[A]
    extension [A](fa: F[A])
        final override def map[B](f: A => B): F[B] =
            pure(f).ap(fa)

    extension [A, B](ff: F[A => B])
        def ap(fa: F[A]): F[B]
        final infix def <*>(fa: F[A]): F[B] = ap(fa)

    extension [A, B](pair: (F[A], F[B]))
        def map2[C](f: (A, B) => C): F[C] =
            pure((a: A) => (b: B) => f(a, b)).ap(pair._1).ap(pair._2)

end Applicative

trait Monad[F[_]] extends Applicative[F]:
    extension [A](a: A) final def ret: F[A] = pure(a)

    extension [A](fa: F[A])
        def flatMap[B](f: A => F[B]): F[B]
        final infix def >>=[B](f: A => F[B]): F[B] = flatMap(f)
        final infix def >>[B](fb: F[B]): F[B] = flatMap(_ => fb)

    extension [A, B](fab: F[A => B])
        final override def ap(fa: F[A]): F[B] = fab.flatMap: ab =>
            fa.flatMap(a => ab(a).ret)

end Monad

object Monad:
    given Monad[Option] with
        override def pure[A](a: => A): Option[A] = Some(a)
        extension [A](
            fa: Option[A]
        ) override def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)

end Monad

class Identity[A](compute: => A):
    lazy val value = compute
    override def equals(x: Any): Boolean =
        x.isInstanceOf[Identity[?]] && x.asInstanceOf[Identity[?]].value == value

object Identity:
    given Monad[Identity] with
        override def pure[A](a: => A): Identity[A] = Identity(a)
        extension [A](fa: Identity[A]) override def flatMap[B](f: A => Identity[B]): Identity[B] = f(fa.value)
