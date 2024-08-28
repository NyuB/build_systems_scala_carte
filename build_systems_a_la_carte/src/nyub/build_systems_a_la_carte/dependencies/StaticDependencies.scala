package nyub.build_systems_a_la_carte.dependencies

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.monads.Applicative

object StaticDependencies:
    /** @param task
      *   an [[Applicative]] task, i.e. which dependencies do not depend on the result of any dependency
      * @return
      *   the direct dependencies required by this task
      */
    def directDependencies[K, V](task: Task[Applicative, K, V]): Set[K] =
        task(k => Const(Set(k))).deps

    /** @param taskKey
      *   the key associated with the task to discover dependencies from in `tasks`
      * @param tasks
      *   task definitions, should include the task associated with `taskKey`and all of its transitive dependencies
      * @return
      *   the dependencies, including transitive ones, of the task associated with `taskKey` in the `tasks` definitions
      */
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

    /** Accumulate dependency keys
      * @param deps
      *   current accumulated dependencies
      */
    private class Const[K](val deps: Set[K])
    private type ConstApplicative[K] = [R] =>> Const[K]

    private given applicativeConst[K]: Applicative[ConstApplicative[K]] with
        override def pure[A](a: A): ConstApplicative[K][A] = Const(Set.empty)
        extension [A, B](ff: ConstApplicative[K][A => B])
            override def ap(
                fa: ConstApplicative[K][A]
            ): ConstApplicative[K][B] =
                Const(ff.deps ++ fa.deps)
