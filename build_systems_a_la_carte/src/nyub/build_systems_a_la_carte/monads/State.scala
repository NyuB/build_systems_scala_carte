package nyub.build_systems_a_la_carte.monads

/** Describes a statefull computation. It can be executed within an arbitrary initial state to retrieve a result along
  * with the updated state
  *
  * @param compute
  *   mapping from a state to an updated state and a result
  * @tparam S
  *   the type of states this computation can be ran in
  * @tparam A
  *   this computation result's type
  */
final class State[S, A](private val compute: S => (S, A)):

    /** Execute this statefull computation within the initial state `s`
      *
      * @param s
      *   the initial state within which to run this computation
      * @return
      *   this computation applied to `s` along with the updated state
      */
    def runState(s: S): (A, S) =
        val (su, a) = compute(s)
        (a, su)

    /** [[runState]] and return only the updated state
      */
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
        final def modify(f: S => S): F[Unit] = get.flatMap: s =>
            put(f(s))

    end Monad

    object Monad:
        type M[S] = [F[_]] =>> Monad[S, F]
        type T[S] = [A] =>> State[S, A]
        given stateMonad[S]: Monad.M[S][Monad.T[S]] with
            override def pure[A](a: => A): State[S, A] = State.gets(_ => a)
            override def get: T[S][S] = State.get
            override def put(s: S): T[S][Unit] = State.put(s)

            extension [A](fa: State[S, A])
                override def flatMap[B](f: A => State[S, B]): State[S, B] =
                    State: s =>
                        val (a, su) = fa.runState(s)
                        val (b, sb) = f(a).runState(su)
                        (sb, b)

    end Monad

end State
