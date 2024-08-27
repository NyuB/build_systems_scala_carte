package nyub.build_systems_a_la_carte.rebuilders

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.HashModule
import nyub.build_systems_a_la_carte.monads.Monad

trait VerifyingTrace[K, V](using val hashModule: HashModule[V]):
    type Hash = hashModule.Hash
    def recordTrace(key: K, hash: Hash, depsHash: Set[(K, Hash)]): VerifyingTrace[K, V]
    def traceChanged[F[_]: Monad](key: K, hash: Hash, getHash: K => F[Hash]): F[Boolean]
