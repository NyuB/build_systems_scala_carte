package nyub.build_systems_a_la_carte.rebuilders

import nyub.build_systems_a_la_carte.HashModule
import nyub.build_systems_a_la_carte.monads.Monad

trait VerifyingTrace[K, V, H](using val hashModule: HashModule[V, H]):
    def recordTrace(key: K, hash: H, depsHash: Set[(K, H)]): VerifyingTrace[K, V, H]
    def traceChanged[F[_]: Monad](key: K, hash: H, getHash: K => F[H]): F[Boolean]

object VerifyingTrace:
    class InMemory[K, V, H](using HashModule[V, H])(private val traces: Map[K, (H, Set[(K, H)])])
        extends VerifyingTrace[K, V, H]:
        override def recordTrace(key: K, hash: H, depsHash: Set[(K, H)]): InMemory[K, V, H] =
            InMemory(traces.updated(key, (hash, depsHash)))

        override def traceChanged[F[_]: Monad](key: K, newHash: H, getHash: K => F[H]): F[Boolean] =
            traces
                .get(key)
                .map: (previousHash, previousDepsHashes) =>
                    previousDepsHashes
                        .map((k, _) => k -> getHash(k))
                        .foldLeft(Set.empty[(K, H)].ret): (fset, f) =>
                            fset.flatMap: set =>
                                f._2.map: entry =>
                                    set + (f._1 -> entry)
                        .map: newDepsHashes =>
                            newHash -> newDepsHashes != previousHash -> previousDepsHashes
                .getOrElse(true.ret)

    object InMemory:
        def init[K, V, H](using HashModule[V, H]): InMemory[K, V, H] = InMemory(Map.empty)
