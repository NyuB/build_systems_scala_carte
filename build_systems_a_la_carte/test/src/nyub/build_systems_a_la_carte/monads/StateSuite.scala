package nyub.build_systems_a_la_carte.monads

class StateSuite extends munit.FunSuite:

    test("String state, appending successive lengths"):
        given Monad[State.Monad.T[String]] = State.Monad.stateMonad[String]
        def appendCurrentStateLength: State[String, Unit] =
            State
                .gets[String, Int](_.length())
                .flatMap(l => State.modify(s => s"$s$l, "))
        def surroundWithBrackets(s: String): State[String, Unit] =
            State.get.flatMap(s => State.put(s"[$s]"))
        val computation =
            appendCurrentStateLength
                >> appendCurrentStateLength
                >> appendCurrentStateLength
                >> appendCurrentStateLength
                >> appendCurrentStateLength
                >> appendCurrentStateLength
                >> State.get[String]
                >>= surroundWithBrackets

        assertEquals(computation.execState(""), "[0, 3, 6, 9, 12, 16, ]")

end StateSuite
