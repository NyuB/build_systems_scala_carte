package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Rebuilder
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Scheduler
import nyub.build_systems_a_la_carte.monads.{Applicative, State}
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.graphs.DAG

type Time = Int
case class MakeInfo[K](now: Time, modificationTimes: Map[K, Time]):
    def update(key: K): MakeInfo[K] =
        MakeInfo(now + 1, modificationTimes.updated(key, now))

class ModTimeRebuilder[K, V] extends Rebuilder[Applicative, MakeInfo[K], K, V]:
    override def rebuild(key: K, task: Task[Applicative, K, V], lastValue: V): Task[State.Monad.M[MakeInfo[K]], K, V] =
        Task:
            [F[_]] =>
                monad ?=>
                    fetch =>
                        val deps = StaticDependencies.directDependencies(task)
                        monad.get.flatMap: info =>
                            val dirty = info.modificationTimes
                                .get(key)
                                .map: modif =>
                                    deps.map(info.modificationTimes.get(_)).present.exists(_ >= modif)
                                .getOrElse(true)
                            if !dirty then lastValue.ret
                            else monad.put(info.update(key)) >> task.run(fetch)

class TopologicalScheduler[I, K, V](using Ordering[K]) extends Scheduler[Applicative, I, I, K, V]:
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

def make[K, V](using Ordering[K]) = TopologicalScheduler[MakeInfo[K], K, V].buildSystem(ModTimeRebuilder[K, V]())

extension [T](l: Iterable[Option[T]])
    def present: Iterable[T] = l.collect:
        case Some(t) => t
