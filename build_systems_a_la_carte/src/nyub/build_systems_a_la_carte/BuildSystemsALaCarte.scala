package nyub.build_systems_a_la_carte

import monads.{Identity, Monad}

object BuildSystemsALaCarte:
    trait StoreModule[I, K, V]:
        type Store
        def initialise(i: I, kv: (K => V)): Store
        def getInfo(store: Store): I
        def putInfo(info: I, store: Store): Store
        def getValue(key: K, store: Store): V
        def putValue(using CanEqual[K, K])(key: K, v: V, store: Store): Store
    end StoreModule

    trait HashModule[V]:
        type Hash
        def hash[T <: V](value: T): Hash
        def getHash[K](key: K, storeModule: StoreModule[?, K, V]): Hash
    end HashModule

    type Task[C[_[_]], K, V] = [F[_]] => (C[F] ?=> (K => F[V]) => F[V])
    def compute[K, V](task: Task[Monad, K, V], storeModule: StoreModule[?, K, V], store: storeModule.Store): V =
        given Monad[Identity] = monads.Identity.given_Monad_Identity
        task(k => Identity(storeModule.getValue(k, store))).value

    type Tasks[C[_[_]], K, V] = K => Option[Task[C, K, V]]

    trait BuildSystem[C[_[_]], I, K, V]:
        def build(
            tasks: Tasks[C, K, V],
            key: K,
            storeModule: StoreModule[I, K, V],
            store: storeModule.Store
        ): storeModule.Store

    end BuildSystem

end BuildSystemsALaCarte
