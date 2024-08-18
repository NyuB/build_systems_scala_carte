package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task

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

    test("Example 3.3 - 'busy' is not a minimal build system"):
        // Given
        val store =
            Example_3_3.BusyBuildModule.s.initialise((), doNotUseDefaultValue)
        val b1Observer = TaskObserver(Example_3_2.taskB1)
        val replaceB2ByNonMinimalVersionAndObserveB1 = (s: String) =>
            s match
                case "B1" => Some(b1Observer.task)
                case "B2" => Some(Example_3_2.taskB2_nonMinimal)
                case k    => Example_3_2.sprsh1(k)

        // When
        val result =
            Example_3_3.BusyBuildModule.busy(
              replaceB2ByNonMinimalVersionAndObserveB1,
              "B2",
              store
            )
        val b1 = Example_3_3.BusyBuildModule.s.getValue("B1", result)
        val b2 = Example_3_3.BusyBuildModule.s.getValue("B2", result)
        assertEquals(
          b1,
          30,
          "observed B1 should have the same result as the raw task"
        )
        assertEquals(b2, 60, "B1 + B1 should be equal to 2 * B1")

        // Then
        assertEquals(
          b1Observer.callCount,
          2,
          "Expected B to be called twice because this build system is not minimal"
        )

    def doNotUseDefaultValue[K, V](k: K): V = ???

    class TaskObserver[C[_[_]], K, V](
        private val underlyingTask: Task[C, K, V]
    ):
        var callCount: Int = 0
        def task: Task[C, K, V] = [F[_]] =>
            constraints ?=>
                fetch =>
                    callCount += 1
                    underlyingTask[F](fetch)

end BuildSuite
