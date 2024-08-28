package nyub.build_systems_a_la_carte

import monads.{Identity, Monad}
import nyub.build_systems_a_la_carte.monads.State

object BuildSystemsALaCarte:
    /** @tparam C
      *   the environment **c**onstraints and **c**apabilities required to run this [[Task]]
      * @tparam K
      *   the **k**eys allowing to fetch other tasks' results
      * @tparam V
      *   the **t**ype of values produced by this task
      */
    trait Task[-C[_[_]], K, V]:
        /** @tparam F
          *   the environment (or e**f**fect) within which to run this task
          * @param constraints
          *   provided **c**onstraints and **c**apabilities in the environment `F[_]`
          * @param fetch
          *   callback to retrieve other tasks' results
          * @return
          *   the result of running this task within the environment `F[_]`
          */
        def run[F[_]](using constraints: C[F])(fetch: K => F[V]): F[V]
        final def apply[F[_]](using constraints: C[F])(fetch: K => F[V]): F[V] = run(fetch)
    end Task

    object Task:
        /** @param storeModule
          *   operations available on `store`
          * @param task
          *   the task to compute
          * @param store
          *   key-value store
          * @return
          *   the result of `task` when each dependency result is directly taken from the `store`
          */
        def compute[K, V](using
            storeModule: StoreModule[?, K, V]
        )(task: Task[Monad, K, V], store: storeModule.Store): V =
            given Monad[Identity] = monads.Identity.given_Monad_Identity
            task(k => Identity(store.getValue(k))).value

        /** @see
          *   [[Task]]
          * @tparam C
          *   the environment **c**onstraints and **c**apabilities required to run this [[Task]]
          * @tparam K
          *   the **k**eys allowing to fetch other tasks' results
          * @tparam V
          *   the **t**ype of values produced by this task
          * @param f
          *   the task implementation
          * @return
          *   a [[Task]] using `f` as implementation
          */
        def apply[C[_[_]], K, V](f: [F[_]] => C[F] ?=> (K => F[V]) => F[V]): Task[C, K, V] = new:
            override def run[F[_]](using constraints: C[F])(fetch: K => F[V]): F[V] = f(fetch)

    end Task

    /** [[Task]] definitions
      * @tparam C
      *   the environment **c**onstraints and **c**apabilities required to run these [[Task]]s
      * @tparam K
      *   the **k**eys allowing to fetch tasks
      * @tparam V
      *   the **t**ype of values produced by these tasks
      */
    trait Tasks[-C[_[_]], K, V]:
        /** @param taskKey
          *   identifier of the task within these [[Tasks]] definition
          * @return
          *   the task associated with `taskKey` if defined within these [[Tasks]], `None` otherwise
          */
        def get(taskKey: K): Option[Task[C, K, V]]

        /** @see
          *   [[Tasks#get]]
          * @return
          *   `get(taskKey)`
          */
        final def apply(taskKey: K): Option[Task[C, K, V]] = get(taskKey)
    end Tasks

    object Tasks:
        /** @param f
          *   `K`ey to [[Task]] definitions
          * @return
          *   an implementation of [[Tasks]] using `f`
          */
        def apply[C[_[_]], K, V](f: K => Option[Task[C, K, V]]): Tasks[C, K, V] = new:
            override def get(taskKey: K): Option[Task[C, K, V]] = f(taskKey)

    end Tasks

    /** A build system, able to schedule, run and update a store with the result of user-defined tasks in arbitrary
      * environments
      *
      * @tparam C
      *   the constraints on the environment within which this build system can execute tasks
      * @tparam I
      *   the meta information that this build system uses from the store
      * @tparam K
      *   the tasks' keys type
      * @tparam V
      *   the tasks' return type
      */
    trait BuildSystem[+C[_[_]], I, K, V]:
        /** @param storeModule
          *   operations available on `store`
          * @param tasks
          *   tasks description and association with keys
          * @param key
          *   the key to update the store with
          * @param store
          *   key-value store
          * @return
          *   `store` updated with the result of `key` after executing any required [[Task]] from `tasks`
          */
        def build(using storeModule: StoreModule[I, K, V])(
            tasks: Tasks[C, K, V],
            key: K,
            store: storeModule.Store
        ): storeModule.Store

    end BuildSystem

    /** Task wrapper, decides if a given task should be re-executed of if it is already up to date
      * @tparam C
      *   the constraints on the environment within which this rebuilder can execute tasks
      * @tparam I
      *   the meta-information this rebuilder uses to decide wether or not and hw to rebuild a task
      * @tparam K
      *   the tasks' keys type
      * @tparam K
      *   the tasks' return type
      */
    trait Rebuilder[C[_[_]], I, K, V]:
        /** Wraps a task in another task using meta-information to decide if a given task should be re-executed of if it
          * is already up to date
          * @param key
          *   the task's key
          * @param task
          *   user-defined task
          * @param lastValue
          *   the last known value of this task
          * @return
          *   a new task wrapping the supplied task, which decides based on supplied meta-information wether or not to
          *   re-execute the underlying task or apply some form of reuse of previous results
          */
        def rebuild(key: K, task: Task[C, K, V], lastValue: V): Task[State.Monad.M[I], K, V]

    trait Scheduler[C[_[_]], I, IR, K, V]:
        def buildSystem(rebuilder: Rebuilder[C, IR, K, V]): BuildSystem[C, I, K, V]

end BuildSystemsALaCarte
