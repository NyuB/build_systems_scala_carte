package nyub.build_systems_a_la_carte

import monads.{Identity, Monad}

object BuildSystemsALaCarte:
    trait StoreModule[I, K, V]:
        type Store
        def initialise(i: I, kv: (K => V)): Store
        extension (store: Store)
            def getInfo: I
            def putInfo(info: I): Store
            def getValue(key: K): V
            def putValue(using CanEqual[K, K])(key: K, v: V): Store

    end StoreModule

    object StoreModule:
        def initialise[I, K, V](using s: StoreModule[I, K, V])(i: I, kv: (K => V)): s.Store = s.initialise(i, kv)
    end StoreModule

    trait HashModule[V]:
        type Hash
        def hash[T <: V](value: T): Hash
        def getHash[K](key: K, storeModule: StoreModule[?, K, V]): Hash
    end HashModule

    type Task[C[_[_]], K, V] = [F[_]] => (C[F] ?=> (K => F[V]) => F[V])
    def compute[K, V](using storeModule: StoreModule[?, K, V])(task: Task[Monad, K, V], store: storeModule.Store): V =
        given Monad[Identity] = monads.Identity.given_Monad_Identity
        task(k => Identity(store.getValue(k))).value

    type Tasks[C[_[_]], K, V] = K => Option[Task[C, K, V]]

    trait BuildSystem[C[_[_]], I, K, V]:
        def build(using storeModule: StoreModule[I, K, V])(
            tasks: Tasks[C, K, V],
            key: K,
            store: storeModule.Store
        ): storeModule.Store

    end BuildSystem

end BuildSystemsALaCarte
