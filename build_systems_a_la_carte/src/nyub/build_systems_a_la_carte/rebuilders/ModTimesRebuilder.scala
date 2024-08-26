package nyub.build_systems_a_la_carte.rebuilders

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.{Rebuilder, Task}
import nyub.build_systems_a_la_carte.monads.{Applicative, State}
import nyub.build_systems_a_la_carte.StaticDependencies

/** A rebuilder based on keys modification times. Will only re-execute a task if any of its dependency has been modified
  * later of simultaneously according to [[ModificationTimes#modificationTimes]]
  */
class ModTimeRebuilder[K, V] extends Rebuilder[Applicative, ModificationTimes[K], K, V]:
    override def rebuild(
        key: K,
        task: Task[Applicative, K, V],
        lastValue: V
    ): Task[State.Monad.M[ModificationTimes[K]], K, V] =
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
                            else monad.put(info.touch(key)) >> task.run(fetch)

    extension [T](l: Iterable[Option[T]])
        private def present: Iterable[T] = l.collect:
            case Some(t) => t
