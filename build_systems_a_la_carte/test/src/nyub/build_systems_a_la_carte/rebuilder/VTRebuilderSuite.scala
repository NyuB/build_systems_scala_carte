package nyub.build_systems_a_la_carte.rebuilder

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.monads.Monad
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.HashModule
import nyub.build_systems_a_la_carte.rebuilders.VerifyingTrace
import nyub.build_systems_a_la_carte.StoreModule
import nyub.build_systems_a_la_carte.TaskObserver
import nyub.build_systems_a_la_carte.rebuilders.VTRebuilder
import nyub.build_systems_a_la_carte.stores.FunctionalStoreModule
import nyub.build_systems_a_la_carte.monads.Applicative
import nyub.build_systems_a_la_carte.schedulers.FixOrderScheduler
import nyub.build_systems_a_la_carte.hashes.HashCodeModule

class VTRebuilderSuite extends munit.FunSuite:
    type Hash = Int
    given HashModule[Int, Int] = HashCodeModule

    test("Tasks are built once when nothing change"):
        val taskOrder = List(ONE_KEY)
        val testBuildSystem = FixOrderScheduler(taskOrder)
            .buildSystem(VTRebuilder[String, Int, Hash])
        given StoreModule[VerifyingTrace[String, Int, Hash], String, Int] = FunctionalStoreModule()
        val store = StoreModule.initialise(VerifyingTrace.InMemory.init, _ => -42)

        val one = TaskObserver(constant(1))

        val tasks: Tasks[Monad, String, Int] =
            case ONE_KEY => Some(one)
        val r = testBuildSystem.build(tasks, ONE_KEY, store)
        assertEquals(r.getValue(ONE_KEY), 1)

        val r2 = testBuildSystem.build(tasks, ONE_KEY, r)
        assertEquals(r2.getValue(ONE_KEY), 1)

        assertEquals(one.callCount, 1)

    test("Rebuild only when dependencies change"):
        val taskOrder = List(LEFT_KEY, RIGHT_KEY, ADD_KEY, DOUBLE_KEY)
        val testBuildSystem = FixOrderScheduler(taskOrder)
            .buildSystem(VTRebuilder[String, Int, Hash])
        given StoreModule[VerifyingTrace[String, Int, Hash], String, Int] = FunctionalStoreModule()

        val observedDouble = TaskObserver(double(ADD_KEY))
        val observedAdd = TaskObserver(add(LEFT_KEY, RIGHT_KEY))
        val tasks: Tasks[Monad, String, Int] =
            case DOUBLE_KEY => Some(observedDouble)
            case ADD_KEY    => Some(observedAdd)
            case _          => None

        def inputs(k: String): Int = k match
            case LEFT_KEY  => 1
            case RIGHT_KEY => 3
            case _         => -42
        val store = StoreModule.initialise(VerifyingTrace.InMemory.init, inputs)
        val r = testBuildSystem.build(tasks, DOUBLE_KEY, store)
        assertEquals(r.getValue(LEFT_KEY), 1)
        assertEquals(r.getValue(RIGHT_KEY), 3)
        assertEquals(r.getValue(ADD_KEY), 4)
        assertEquals(r.getValue(DOUBLE_KEY), 8)
        assertEquals(observedAdd.callCount, 1)
        assertEquals(observedDouble.callCount, 1)

        val r2 = testBuildSystem.build(tasks, DOUBLE_KEY, r.putValue(LEFT_KEY, 2).putValue(RIGHT_KEY, 2))
        assertEquals(r2.getValue(LEFT_KEY), 2)
        assertEquals(r2.getValue(RIGHT_KEY), 2)
        assertEquals(r2.getValue(ADD_KEY), 4)
        assertEquals(r2.getValue(DOUBLE_KEY), 8)
        assertEquals(observedAdd.callCount, 2)
        assertEquals(observedDouble.callCount, 1)

    private def add(taskA: String, taskB: String): Task[Applicative, String, Int] = Task:
        [F[_]] => app ?=> fetch => ((a: Int) => (b: Int) => a + b) `<$>` fetch(taskA) <*> fetch(taskB)

    private def double(task: String): Task[Applicative, String, Int] = Task:
        [F[_]] => app ?=> fetch => fetch(task).map(_ * 2)

    private def constant(c: Int): Task[Applicative, String, Int] = Task:
        [F[_]] => app ?=> fetch => app.pure(c)

end VTRebuilderSuite

private val ONE_KEY = "One"
private val TWO_KEY = "Two"
private val THREE_KEY = "Three"
private val ADD_KEY = "Add"
private val DOUBLE_KEY = "Double"
private val LEFT_KEY = "Left"
private val RIGHT_KEY = "Right"
