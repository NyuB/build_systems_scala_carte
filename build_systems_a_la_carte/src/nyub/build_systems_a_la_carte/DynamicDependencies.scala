package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.monads.Monad
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task

object DynamicDependencies:
    /** @param task
      *   a [[Monad]]ic task, i.e. which dependencies may depend on the result of any dependency
      * @return
      *   the direct (i.e. non transitive) dependencies required by this task
      */
    def directDependencies[F[_], K, V](using monad: Monad[F])(fetch: K => F[V])(
        task: Task[Monad, K, V]
    ): F[(V, Set[(K, V)])] =
        track(fetch, task)

    private def track[F[_], K, V](using
        monad: Monad[F]
    )(fetch: K => F[V], task: Task[Monad, K, V]): F[(V, Set[(K, V)])] =
        def trackingFetch(k: K): DependenciesAccumulator[K, V, F, V] =
            val v = fetch(k).map(v => (v, Set(k -> v)))
            DependenciesAccumulator(v)
        given Monad[DependenciesAccumulatorMonad[K, V, F]] = dependenciesAccumulatorMonad[K, V, F]
        task(trackingFetch).run

    private class DependenciesAccumulator[K, V, F[_], A](
        private[DynamicDependencies] val underlyingEffect: F[(A, Set[(K, V)])]
    ):
        def run: F[(A, Set[(K, V)])] = underlyingEffect

    private type DependenciesAccumulatorMonad[K, V, F[_]] = [A] =>> DependenciesAccumulator[K, V, F, A]

    private given dependenciesAccumulatorMonad[K, V, F[_]](using
        monad: Monad[F]
    ): Monad[DependenciesAccumulatorMonad[K, V, F]] with
        override def pure[A](a: A): DependenciesAccumulatorMonad[K, V, F][A] = DependenciesAccumulator(
          monad.pure(a -> Set.empty)
        )

        extension [A](fa: DependenciesAccumulatorMonad[K, V, F][A])
            override def flatMap[B](
                f: A => DependenciesAccumulatorMonad[K, V, F][B]
            ): DependenciesAccumulatorMonad[K, V, F][B] =
                val applied = fa.underlyingEffect.flatMap: (a, depsA) =>
                    f(a).underlyingEffect.map: (b, depsB) =>
                        (b, depsA ++ depsB)
                DependenciesAccumulator(applied)

end DynamicDependencies
