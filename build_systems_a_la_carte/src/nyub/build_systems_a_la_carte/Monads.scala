package nyub.build_systems_a_la_carte

trait Functor[F[_]]:
    extension [A](fa: F[A]) def map[B](f: A => B): F[B]
    extension [A, B](f: A => B) final infix def `<$>`(fa: F[A]): F[B] = fa.map(f)

end Functor

trait Applicative[F[_]] extends Functor[F]:
    def pure[A](a: A): F[A]
    extension [A](fa: F[A])
        final override def map[B](f: A => B): F[B] =
            pure(f).ap(fa)

    extension [A, B](ff: F[A => B])
        def ap(fa: F[A]): F[B]
        final infix def <*>(fa: F[A]): F[B] = ap(fa)

end Applicative

trait Monad[F[_]] extends Applicative[F]:
    extension [A](a: A) final def ret: F[A] = pure(a)

    extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]

    extension [A, B](fab: F[A => B])
        final override def ap(fa: F[A]): F[B] = fab.flatMap: ab =>
            fa.flatMap(a => ab(a).ret)

end Monad
