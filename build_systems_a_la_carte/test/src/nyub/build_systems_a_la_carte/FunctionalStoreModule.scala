package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule

class FunctionalStoreModule[I, K, V] extends StoreModule[I, K, V]:
    override type Store = (I, K => V)
    override def initialise(i: I, kv: K => V): Store = (i, kv)
    extension (store: Store) override def getInfo: I = store._1
    extension (store: Store) override def getValue(key: K): V = store._2(key)
    extension (store: Store) override def putInfo(info: I): Store = (info, store._2)
    extension (store: Store)
        override def putValue(using CanEqual[K, K])(key: K, v: V): Store =
            (store._1, (k => if k == key then v else store._2(k)))

object FunctionalStoreModule:
    def defaultValue[I, K, V](using m: FunctionalStoreModule[I, K, V])(initialInfo: I)(defaultValue: V): m.Store =
        m.initialise(initialInfo, _ => defaultValue)

    def failIfNotPut[I, K, V](using m: FunctionalStoreModule[I, K, V])(initialInfo: I): m.Store =
        m.initialise(initialInfo, _ => ???)
