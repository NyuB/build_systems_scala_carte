package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.monads.{monadicState, Applicative, Monad, State, StateMonad}

object Example_3_3:
    object BusyBuildModule extends BuildSystem[Applicative, Unit, String, Int]:
        def build(
            tasks: Tasks[Applicative, String, Int],
            key: String,
            storeModule: StoreModule[Unit, String, Int],
            store: storeModule.Store
        ) =
            given Monad[StateMonad[storeModule.Store]] = monadicState[storeModule.Store]

            def fetch(k: String): State[storeModule.Store, Int] =
                tasks(k) match
                    case None => monads.State.gets(st => storeModule.getValue(k, st))
                    case Some(task) =>
                        task(fetch) >>= storeValueThenReturn(k, storeModule)

            fetch(key).execState(store)

        private def storeValueThenReturn(at: String, storeModule: StoreModule[Unit, String, Int])(
            v: Int
        ): State[storeModule.Store, Int] =
            monads.State.modify(st => storeModule.putValue(at, v, st)) >> v.ret

    class BusyStoreModule(val constants: Map[String, Int]) extends StoreModule[Unit, String, Int]:
        type Store = Map[String, Int]
        override def initialise(i: Unit, kv: String => Int): Store =
            constants.withDefault(kv)

        override def getInfo(store: Map[String, Int]): Unit = ()
        override def getValue(key: String, store: Map[String, Int]): Int =
            store(key)

        override def putInfo(info: Unit, store: Map[String, Int]): Store = store
        override def putValue(using
            CanEqual[String, String]
        )(key: String, v: Int, store: Map[String, Int]): Store =
            store.updated(key, v)

end Example_3_3
