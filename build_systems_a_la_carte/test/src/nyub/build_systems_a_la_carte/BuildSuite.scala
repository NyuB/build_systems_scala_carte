package nyub.build_systems_a_la_carte

class BuildSuite extends munit.FunSuite:
    test("Example 3.3 - compute B1"):
        val store =
            Example_3_3.BusyBuildModule.s.initialise((), doNotUseDefaultValue)
        val result =
            Example_3_3.BusyBuildModule.busy(Example_3_2.sprsh1, "B1", store)
        val b1 = Example_3_3.BusyBuildModule.s.getValue("B1", result)
        assertEquals(b1, 30)

    test("Example 3.3 - compute B2, B1 computed along the way"):
        val store =
            Example_3_3.BusyBuildModule.s.initialise((), doNotUseDefaultValue)
        val result =
            Example_3_3.BusyBuildModule.busy(Example_3_2.sprsh1, "B2", store)
        val b1 = Example_3_3.BusyBuildModule.s.getValue("B1", result)
        val b2 = Example_3_3.BusyBuildModule.s.getValue("B2", result)
        assertEquals(b1, 30)
        assertEquals(b2, 60)

    def doNotUseDefaultValue[K, V](k: K): V = ???

end BuildSuite
