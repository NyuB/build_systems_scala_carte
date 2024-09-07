package nyub.build_systems_a_la_carte.stores

import nyub.build_systems_a_la_carte.StoreModule

/** In-memory [[StoreModule]] implementation using an anonymous function as store
  */
class FunctionalStoreModule[I, K, V] extends StoreModule[I, K, V]:
    /** A tuple of store meta-information and the store function
      */
    override type Store = (I, K => V)

    override def initialise(i: I, kv: K => V): Store = (i, kv)
    extension (store: Store) override def getInfo: I = store._1
    extension (store: Store) override def getValue(key: K): V = store._2(key)
    extension (store: Store) override def putInfo(info: I): Store = (info, store._2)
    extension (store: Store)
        override def putValue(using CanEqual[K, K])(key: K, v: V): Store =
            (store._1, (k => if k == key then v else store._2(k)))

object FunctionalStoreModule:
    /** @return
      *   a store returning the same value for all keys
      */
    def defaultValue[I, K, V](using m: FunctionalStoreModule[I, K, V])(initialInfo: I)(defaultValue: V): m.Store =
        m.initialise(initialInfo, _ => defaultValue)

    /** @return
      *   a store throwing an exception for all keys
      */
    def failIfNotPut[I, K, V](using m: FunctionalStoreModule[I, K, V])(initialInfo: I): m.Store =
        m.initialise(initialInfo, _ => ???)
