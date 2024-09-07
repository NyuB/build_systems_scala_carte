package nyub.makette

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.monads.Applicative

/** @param delegate
  *   a [[MaketteTask]] function with an additional call count
  */
class TaskObserver[V](
    private val delegate: MaketteTask[V]
) extends MaketteTask[V]:
    var callCount: Int = 0

    override def within(workspace: Workspace): Task[Applicative, Key, TaskResult[V]] = new:
        final override def run[F[_]](using constraints: Applicative[F])(
            fetch: Key => F[TaskResult[V]]
        ): F[TaskResult[V]] =
            callCount += 1
            delegate.within(workspace).run(fetch)
