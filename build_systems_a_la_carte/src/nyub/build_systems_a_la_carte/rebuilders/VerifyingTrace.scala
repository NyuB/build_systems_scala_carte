package nyub.build_systems_a_la_carte.rebuilders

import nyub.build_systems_a_la_carte.HashModule
import nyub.build_systems_a_la_carte.monads.Monad

trait VerifyingTrace[K, V, H](using val hashModule: HashModule[V, H]):
    def recordTrace(key: K, hash: H, depsHash: Set[(K, H)]): VerifyingTrace[K, V, H]
    def traceChanged[F[_]: Monad](key: K, hash: H, getHash: K => F[H]): F[Boolean]
