package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.monads.{monadicState, Applicative, Monad, State, StateMonad}

object Example_3_3:
    class BusyBuildSystem[V] extends BuildSystem[Applicative, Unit, String, V]:
        override def build(using storeModule: StoreModule[Unit, String, V])(
            tasks: Tasks[Applicative, String, V],
            key: String,
            store: storeModule.Store
        ) =
            given Monad[StateMonad[storeModule.Store]] = monadicState[storeModule.Store]

            def fetch(k: String): State[storeModule.Store, V] =
                tasks(k) match
                    case None => monads.State.gets(st => st.getValue(k))
                    case Some(task) =>
                        task(fetch) >>= storeValueThenReturn(k)

            fetch(key).execState(store)

        private def storeValueThenReturn(using storeModule: StoreModule[Unit, String, V])(at: String)(
            v: V
        ): State[storeModule.Store, V] =
            monads.State.modify[storeModule.Store](st => st.putValue(at, v)) >> v.ret

    class BusyStoreModule(val constants: Map[String, Int]) extends StoreModule[Unit, String, Int]:
        type Store = Map[String, Int]
        override def initialise(i: Unit, kv: String => Int): Store =
            constants.withDefault(kv)

        extension (store: Map[String, Int])
            override def getInfo: Unit = ()
            override def getValue(key: String): Int =
                store(key)

            override def putInfo(info: Unit): Store = store
            override def putValue(using
                CanEqual[String, String]
            )(key: String, v: Int): Store =
                store.updated(key, v)

end Example_3_3
