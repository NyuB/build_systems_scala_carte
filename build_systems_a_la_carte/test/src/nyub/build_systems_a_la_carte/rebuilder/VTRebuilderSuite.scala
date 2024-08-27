package nyub.build_systems_a_la_carte.rebuilder

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.monads.Monad
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.hashes.HashModule
import nyub.build_systems_a_la_carte.rebuilders.VerifyingTrace
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Scheduler
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Rebuilder
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule
import nyub.build_systems_a_la_carte.TaskObserver
import nyub.build_systems_a_la_carte.monads.State
import nyub.build_systems_a_la_carte.rebuilders.VTRebuilder
import nyub.build_systems_a_la_carte.FunctionalStoreModule
import nyub.build_systems_a_la_carte.monads.Applicative

class VTRebuilderSuite extends munit.FunSuite:
    type Hash = Int
    given HashModule[Int, Int] = IntHash

    test("Tasks are built once when nothing change"):
        val taskOrder = List(ONE_KEY)
        val testBuildSystem = FixScheduler[VerifyingTrace[String, Int, Hash], String, Int](taskOrder)
            .buildSystem(VTRebuilder[String, Int, Hash])
        given StoreModule[VerifyingTrace[String, Int, Hash], String, Int] = FunctionalStoreModule()
        val store = StoreModule.initialise(IntTrace.init, _ => -42)

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
        val testBuildSystem = FixScheduler[VerifyingTrace[String, Int, Hash], String, Int](taskOrder)
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
        val store = StoreModule.initialise(IntTrace.init, inputs)
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

    private class IntTrace(private val traces: Map[String, (Hash, Set[(String, Hash)])])
        extends VerifyingTrace[String, Int, Hash]:
        override def recordTrace(key: String, hash: Hash, depsHash: Set[(String, Hash)]): IntTrace =
            IntTrace(traces.updated(key, (hash, depsHash)))

        override def traceChanged[F[_]: Monad](key: String, newHash: Hash, getHash: String => F[Hash]): F[Boolean] =
            traces
                .get(key)
                .map: (previousHash, previousDepsHashes) =>
                    previousDepsHashes
                        .map((k, _) => k -> getHash(k))
                        .foldLeft(Set.empty[(String, Hash)].ret): (fset, f) =>
                            fset.flatMap: set =>
                                f._2.map: entry =>
                                    set + (f._1 -> entry)
                        .map(newDepsHashes => (newHash, newDepsHashes) != (previousHash, previousDepsHashes))
                .getOrElse(true.ret)

    private object IntTrace:
        def init = IntTrace(Map.empty)

    private class FixScheduler[I, K, V](private val order: List[K]) extends Scheduler[Monad, I, I, K, V]:
        override def buildSystem(rebuilder: Rebuilder[Monad, I, K, V]): BuildSystem[Monad, I, K, V] = new:
            override def build(using
                storeModule: StoreModule[I, K, V]
            )(tasks: Tasks[Monad, K, V], key: K, store: storeModule.Store): storeModule.Store =
                given Monad[State.Monad.T[storeModule.Store]] = State.Monad.stateMonad[storeModule.Store]
                order
                    .foldLeft(().ret): (acc, item) =>
                        acc >> rebuilder.build(tasks, item)
                    .execState(store)

    object IntHash extends HashModule[Int, Hash]:
        override def hash[I <: Int](i: I): Hash = i

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
