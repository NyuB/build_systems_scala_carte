package nyub.build_systems_a_la_carte

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
