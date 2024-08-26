package nyub.build_systems_a_la_carte.schedulers

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.{BuildSystem, Rebuilder, Scheduler, StoreModule, Tasks}
import nyub.build_systems_a_la_carte.monads.{Applicative, State}
import nyub.build_systems_a_la_carte.graphs.DAG
import nyub.build_systems_a_la_carte.StaticDependencies

/** A scheduler using a topological ordering: leaves tasks are scheduled first, then their dependants tasks, etc
  *
  * Since this requires static knowledge of a given task dependencies, this scheduler accept only [[Applicative]] tasks
  *
  * @param ordering
  *   the ordering used to sort keys that are at the same topological level
  */
class TopologicalScheduler[I, K, V](using ordering: Ordering[K]) extends Scheduler[Applicative, I, I, K, V]:
    override def buildSystem(rebuilder: Rebuilder[Applicative, I, K, V]): BuildSystem[Applicative, I, K, V] = new:
        override def build(using
            storeModule: StoreModule[I, K, V]
        )(tasks: Tasks[Applicative, K, V], key: K, store: storeModule.Store): storeModule.Store =
            given State.Monad.M[storeModule.Store][State.Monad.T[storeModule.Store]] =
                State.Monad.stateMonad[storeModule.Store]
            def buildInternal(key: K): State[storeModule.Store, Unit] = tasks(key) match
                case None => ().ret
                case Some(task) =>
                    State.get.flatMap: store =>
                        val value = store.getValue(key)
                        val newTask = rebuilder.rebuild(key, task, value)
                        given State.Monad.M[I][State.Monad.T[I]] = State.Monad.stateMonad[I]
                        def fetch(k: K): State[I, V] = (store.getValue(k).ret)
                        liftStore(newTask.run(fetch)).flatMap: newValue =>
                            State.modify: s =>
                                s.putValue(key, newValue)
            val dag = DAG(
              key,
              k => tasks(k).map(t => StaticDependencies.directDependencies(t)).getOrElse(Set.empty)
            )
            val order = dag.topologicalOrder
            order
                .foldLeft(().ret): (acc, item) =>
                    acc >> buildInternal(item)
                .execState(store)

    private def liftStore[A](using storeModule: StoreModule[I, K, V])(state: State[I, A]): State[storeModule.Store, A] =
        given State.Monad.M[storeModule.Store][State.Monad.T[storeModule.Store]] =
            State.Monad.stateMonad[storeModule.Store]
        State
            .gets[storeModule.Store, (A, I)](s => state.runState(s.getInfo))
            .flatMap: (a, newInfo) =>
                State.modify[storeModule.Store](s => s.putInfo(newInfo)) >> a.ret
