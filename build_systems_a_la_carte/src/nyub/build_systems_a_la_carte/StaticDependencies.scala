package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

object StaticDependencies:
    def directDependencies[K, V](task: Task[Applicative, K, V]): Set[K] =
        task(k => Const(Set(k))).deps

    def dependencies[K, V](
        taskKey: K,
        tasks: Tasks[Applicative, K, V]
    ): Set[K] = dependencies(Set.empty, taskKey, tasks)

    private def dependencies[K, V](
        already: Set[K],
        taskKey: K,
        tasks: Tasks[Applicative, K, V]
    ): Set[K] =
        tasks(taskKey) match
            case None => Set.empty
            case Some(task) =>
                val direct = directDependencies(task)
                direct
                    .filterNot(already)
                    .foldLeft(direct): (acc, item) =>
                        acc ++ dependencies(already + taskKey, item, tasks)

    private class Const[K](val deps: Set[K])
    private type ConstApplicative[K] = [R] =>> Const[K]

    private given applicativeConst[K]: Applicative[ConstApplicative[K]] with
        override def pure[A](a: A): ConstApplicative[K][A] = Const(Set.empty)
        extension [A, B](ff: ConstApplicative[K][A => B])
            override def ap(
                fa: ConstApplicative[K][A]
            ): ConstApplicative[K][B] =
                Const(ff.deps ++ fa.deps)
