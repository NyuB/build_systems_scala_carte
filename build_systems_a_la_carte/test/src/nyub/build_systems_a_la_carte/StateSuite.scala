package nyub.build_systems_a_la_carte

class StateSuite extends munit.FunSuite:

    test("String state, appending successive lengths"):
        given Monad[StateMonad[String]] = monadicState[String]
        def appendCurrentStateLength: State[String, Unit] =
            State
                .gets[String, Int](_.length())
                .flatMap(l => State.modify(s => s"$s$l, "))
        def surroundWithBrackets(s: String): State[String, Unit] =
            State.get.flatMap(s => State.put(s"[$s]"))
        val computation =
            appendCurrentStateLength
                .`and then`(appendCurrentStateLength)
                .`and then`(appendCurrentStateLength)
                .`and then`(appendCurrentStateLength)
                .`and then`(appendCurrentStateLength)
                .`and then`(appendCurrentStateLength)
                .`and then`(State.get[String])
                .`and then`(surroundWithBrackets)

        assertEquals(computation.execState(""), "[0, 3, 6, 9, 12, 16, ]")

    extension [S, A](state: State[S, A])
        def `and then`[B](next: State[S, B]): State[S, B] =
            state.flatMap(_ => next)

        def `and then`[B](f: A => State[S, B]): State[S, B] =
            state.flatMap(f)

end StateSuite
