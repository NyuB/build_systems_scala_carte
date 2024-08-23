package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.monads.Monad

class BuildSystemSuite extends munit.FunSuite:

    test("Example 2.1 - compute main.exe"):
        given Example_2_1.storeModule.type = Example_2_1.storeModule
        val result = Example_3_3.BusyBuildSystem().build(Example_2_1.tasks, "main.exe", Example_2_1.inputs)
        assertEquals(
          result.getValue("main.exe"),
          Example_2_1.Artifact.Executable(
            "[EXE - [OBJ - [HEADER - util][SOURCE - main]][OBJ - [HEADER - util][SOURCE - util]]]"
          )
        )

    test("Example 3.3 - compute B1"):
        given StoreModule[Unit, String, Int] = spreadSheetInputs
        val store = StoreModule.initialise((), doNotUseDefaultValue)
        val result =
            Example_3_3.BusyBuildSystem().build(Example_3_2.sprsh1, "B1", store)
        val b1 = result.getValue("B1")
        assertEquals(b1, 30)

    test("Example 3.3 - compute B2, B1 computed along the way"):
        given StoreModule[Unit, String, Int] = spreadSheetInputs
        val store = StoreModule.initialise((), doNotUseDefaultValue)
        val result =
            Example_3_3.BusyBuildSystem().build(Example_3_2.sprsh1, "B2", store)
        val b1 = result.getValue("B1")
        val b2 = result.getValue("B2")
        assertEquals(b1, 30)
        assertEquals(b2, 60)

    test("Example 3.3 - 'busy' is not a minimal build system"):
        // Given
        given StoreModule[Unit, String, Int] = spreadSheetInputs
        val store = StoreModule.initialise((), doNotUseDefaultValue)
        val b1Observer = TaskObserver(Example_3_2.taskB1)
        val replaceB2ByNonMinimalVersionAndObserveB1 = (s: String) =>
            s match
                case "B1" => Some(b1Observer)
                case "B2" => Some(Example_3_2.taskB2_nonMinimal)
                case k    => Example_3_2.sprsh1(k)

        // When
        val result =
            Example_3_3
                .BusyBuildSystem()
                .build(
                  Tasks(replaceB2ByNonMinimalVersionAndObserveB1),
                  "B2",
                  store
                )
        assertEquals(
          result.getValue("B1"),
          30,
          "observed B1 should have the same result as the raw task"
        )
        assertEquals(result.getValue("B2"), 60, "B1 + B1 should be equal to 2 * B1")

        // Then
        assertEquals(
          b1Observer.callCount,
          2,
          "Expected B to be called twice because this build system is not minimal"
        )

    test("Monadic build systems are applicative build systems"):
        given FunctionalStoreModule[Unit, String, Int] = FunctionalStoreModule()
        DoNothingBuildSystem.build(Example_3_2.sprsh1, "B1", FunctionalStoreModule.failIfNotPut(()))

    private val spreadSheetInputs = Example_3_3.BusyStoreModule(
      Map("A1" -> 10, "A2" -> 20)
    )

    private def doNotUseDefaultValue[K, V](k: K): V = ???

    private class TaskObserver[-C[_[_]], K, V](
        private val underlyingTask: Task[C, K, V]
    ) extends Task[C, K, V]:
        var callCount: Int = 0
        override def run[F[_]](using constraints: C[F])(fetch: K => F[V]): F[V] =
            callCount += 1
            underlyingTask(fetch)

    private object DoNothingBuildSystem extends BuildSystem[Monad, Unit, String, Int]:
        override def build(using
            storeModule: StoreModule[Unit, String, Int]
        )(tasks: Tasks[Monad, String, Int], key: String, store: storeModule.Store): storeModule.Store =
            store

end BuildSystemSuite
