package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task

/** @param delegate
  *   a [[Task]] function with an additional `called` callback that can be used to signal when actual task work is done
  */
class TaskObserver[-C[_[_]], K, V] private (
    private val delegate: [F[_]] => C[F] ?=> (K => F[V]) => (() => Unit) => F[V]
) extends Task[C, K, V]:
    var callCount: Int = 0

    final override def run[F[_]](using constraints: C[F])(fetch: K => F[V]): F[V] =
        delegate(fetch)(() => callCount += 1)

object TaskObserver:
    /** @param task
      *   the underlying task to observe
      * @return
      *   an observed task incrementing it's call counter at each [[Task#run]] call
      */
    def apply[C[_[_]], K, V](task: Task[C, K, V]): TaskObserver[C, K, V] =
        val delegate: [F[_]] => C[F] ?=> (K => F[V]) => (() => Unit) => F[V] =
            [F[_]] =>
                _ ?=>
                    fetch =>
                        called =>
                            called()
                            task.run(fetch)
        new TaskObserver(delegate)

    /** @param delegate
      *   a [[Task]] function with an additional `called` callback that can be used to signal when actual task work is
      *   done
      * @return
      *   an observed task based on the supplied `delegate`
      */
    def observe[C[_[_]], K, V](
        delegate: [F[_]] => C[F] ?=> (K => F[V]) => (() => Unit) => F[V]
    ): TaskObserver[C, K, V] = new TaskObserver(delegate)
