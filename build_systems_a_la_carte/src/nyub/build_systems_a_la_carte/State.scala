package nyub.build_systems_a_la_carte

final class State[S, A](val compute: S => (S, A)):
    def get: State[S, S] = State: s =>
        val (su, _) = compute(s)
        (su, su)

    def gets[B](f: S => B): State[S, B] = State: s =>
        val (su, _) = compute(s)
        (su, f(su))

    def put(su: S): State[S, Unit] = State: s =>
        (su, ())

    def modify(f: S => S): State[S, Unit] = State: s =>
        val (su, _) = compute(s)
        (f(su), ())

    def runState(s: S): (A, S) =
        val (su, a) = compute(s)
        (a, su)

    def execState(s: S): S = runState(s)._2
end State

object State:
    def get[S]: State[S, S] = State(s => (s, s))
end State

type StateMonad[S] = [A] =>> State[S, A]

given monadicState[S]: Monad[StateMonad[S]] with
    extension [A](a: A) override def ret: State[S, A] = State(s => (s, a))
    extension [A](fa: State[S, A])
        override def flatMap[B](f: A => State[S, B]): State[S, B] =
            State: s =>
                val (su, a) = fa.compute(s)
                f(a).compute(su)

/*
get :: State s s
gets :: (s -> a) -> State s a
put :: s -> State s ()
modify :: (s -> s) -> State s ()
runState :: State s a -> s -> (a, s)
execState :: State s a -> s -> s
 */
