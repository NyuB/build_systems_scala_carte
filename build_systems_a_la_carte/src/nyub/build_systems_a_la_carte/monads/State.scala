package nyub.build_systems_a_la_carte.monads

final class State[S, A](val compute: S => (S, A)):
    def runState(s: S): (A, S) =
        val (su, a) = compute(s)
        (a, su)

    def execState(s: S): S = runState(s)._2
end State

object State:
    def get[S]: State[S, S] = State(s => (s, s))
    def gets[S, B](f: S => B): State[S, B] = State(s => (s, f(s)))
    def modify[S](f: S => S): State[S, Unit] = State: s =>
        (f(s), ())

    def put[S](s: S): State[S, Unit] = State: _ =>
        (s, ())

    trait Monad[S, F[_]] extends nyub.build_systems_a_la_carte.monads.Monad[F]:
        def get: F[S]
        def put(s: S): F[Unit]
    end Monad

    object Monad:
        type M[S] = [F[_]] =>> Monad[S, F]
        type T[S] = [A] =>> State[S, A]
        given stateMonad[S]: Monad.M[S][Monad.T[S]] with
            override def pure[A](a: A): State[S, A] = State.gets(_ => a)
            override def get: T[S][S] = State.get
            override def put(s: S): T[S][Unit] = State.put(s)

            extension [A](fa: State[S, A])
                override def flatMap[B](f: A => State[S, B]): State[S, B] =
                    State: s =>
                        val (su, a) = fa.compute(s)
                        f(a).compute(su)

    end Monad

end State
