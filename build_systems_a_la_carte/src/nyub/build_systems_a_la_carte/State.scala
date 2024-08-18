package nyub.build_systems_a_la_carte

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

end State

type StateMonad[S] = [A] =>> State[S, A]

given monadicState[S]: Monad[StateMonad[S]] with
    override def pure[A](a: A): State[S, A] = State.gets(_ => a)
    extension [A](fa: State[S, A])
        override def flatMap[B](f: A => State[S, B]): State[S, B] =
            State: s =>
                val (su, a) = fa.compute(s)
                f(a).compute(su)
