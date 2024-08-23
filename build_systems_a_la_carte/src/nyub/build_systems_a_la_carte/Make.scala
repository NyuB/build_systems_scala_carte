package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Rebuilder
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Scheduler
import nyub.build_systems_a_la_carte.monads.{Applicative, State}
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem

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
                                    deps.map(info.modificationTimes.get(_)).present.exists(_ > modif)
                                .getOrElse(true)
                            if !dirty then lastValue.ret
                            else monad.put(info.update(key)) >> task.run(fetch)

class TopologicalScheduler[I, K, V] extends Scheduler[Applicative, I, I, K, V]:
    override def buildSystem(rebuilder: Rebuilder[Applicative, I, K, V]): BuildSystem[Applicative, I, K, V] = ???

def make[K, V] = TopologicalScheduler[MakeInfo[K], K, V].buildSystem(ModTimeRebuilder[K, V]())

extension [T](l: Iterable[Option[T]])
    def present: Iterable[T] = l.collect:
        case Some(t) => t
