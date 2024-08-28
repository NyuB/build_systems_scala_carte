package nyub.build_systems_a_la_carte.schedulers

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Scheduler
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Rebuilder
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.monads.State
import nyub.build_systems_a_la_carte.monads.State.Monad.stateMonad
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task

/** A scheduler that will always build the same tasks in the same order
  *
  * @param order
  *   the tasks to run at each build, in order
  */
class FixOrderScheduler[C[_[_]], I, K, V](private val order: Iterable[K]) extends Scheduler[C, I, I, K, V]:
    override def buildSystem(rebuilder: Rebuilder[C, I, K, V]): BuildSystem[C, I, K, V] = new:
        override def build(using
            storeModule: StoreModule[I, K, V]
        )(tasks: Tasks[C, K, V], key: K, store: storeModule.Store): storeModule.Store =
            order
                .foldLeft(().ret)((state, item) => state >> internalBuild(tasks, item))
                .execState(store)

        final private def internalBuild(using
            storeModule: StoreModule[I, K, V]
        )(tasks: Tasks[C, K, V], key: K): State[storeModule.Store, Unit] =
            tasks(key) match
                case None => ().ret
                case Some(task) =>
                    State.get.flatMap(rebuildTask(key, task))

        final private def rebuildTask(using storeModule: StoreModule[I, K, V])(key: K, task: Task[C, K, V])(
            store: storeModule.Store
        ) =
            val value = store.getValue(key)
            val newTask = rebuilder.rebuild(key, task, value)
            given State.Monad.M[I][State.Monad.T[I]] = State.Monad.stateMonad[I]
            def fetch(k: K): State[I, V] = store.getValue(k).ret
            liftStore(newTask.run(fetch)) >>= putNewValue(key)

        final private def putNewValue(using
            storeModule: StoreModule[I, K, V]
        )(key: K)(newValue: V): State[storeModule.Store, Unit] =
            State.modify: s =>
                s.putValue(key, newValue)

        private def liftStore[A](using storeModule: StoreModule[I, K, V])(
            state: State[I, A]
        ): State[storeModule.Store, A] =
            State
                .gets[storeModule.Store, (A, I)](s => state.runState(s.getInfo))
                .flatMap: (a, newInfo) =>
                    State.modify[storeModule.Store](s => s.putInfo(newInfo)) >> a.ret
