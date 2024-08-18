package nyub.build_systems_a_la_carte

import monads.{Identity, Monad}

object BuildSystemsALaCarte:
    /** Key-Value store operations
      */
    trait StoreModule[I, K, V]:
        /** The actual store type */
        type Store

        /** @param i
          *   initial persistent information
          * @param kv
          *   initial key-value mapping for the created store
          * @return
          *   a new [[Store]] with initial information and default key-value mapping
          */
        def initialise(i: I, kv: (K => V)): Store

        extension (store: Store)
            /** @return
              *   the persistent information associated with this `store`
              */
            def getInfo: I
            def putInfo(info: I): Store
            def getValue(key: K): V
            def putValue(using CanEqual[K, K])(key: K, v: V): Store

    end StoreModule

    object StoreModule:
        /** @see StoreModule#initialise */
        def initialise[I, K, V](using s: StoreModule[I, K, V])(i: I, kv: (K => V)): s.Store = s.initialise(i, kv)
    end StoreModule

    trait HashModule[V]:
        type Hash
        def hash[T <: V](value: T): Hash
        def getHash[K](key: K, storeModule: StoreModule[?, K, V]): Hash
    end HashModule

    type Task[C[_[_]], K, V] = [F[_]] => (C[F] ?=> (K => F[V]) => F[V])
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

    end Task

    type Tasks[C[_[_]], K, V] = K => Option[Task[C, K, V]]

    trait BuildSystem[C[_[_]], I, K, V]:
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

end BuildSystemsALaCarte
