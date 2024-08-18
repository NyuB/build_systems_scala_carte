package nyub.build_systems_a_la_carte

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
        given Monad[Identity] = Identity.given_Monad_Identity
        task(k => Identity(storeModule.getValue(k, store))).value

    type Tasks[C[_[_]], K, V] = K => Option[Task[C, K, V]]

    trait BuildModule[C[_[_]], I, K, V]:
        val s: StoreModule[I, K, V]
        type Build = (Tasks[C, K, V], K, s.Store) => s.Store
    end BuildModule

end BuildSystemsALaCarte
