package nyub.build_systems_a_la_carte

class StateSuite extends munit.FunSuite:

    test("String state, appending successive lengths"):
        given Monad[StateMonad[String]] = monadicState[String]
        extension (s: State[String, ?])
            def appendCurrentStateLength: State[String, Unit] =
                s.gets(_.length())
                    .flatMap(l => l.ret.modify(s => s"$s$l, "))
        val computation =
            State.get.appendCurrentStateLength.appendCurrentStateLength.appendCurrentStateLength.appendCurrentStateLength.appendCurrentStateLength.appendCurrentStateLength
                .modify(s => s"[$s]")
                .get
        assertEquals(computation.execState(""), "[0, 3, 6, 9, 12, 16, ]")

end StateSuite
