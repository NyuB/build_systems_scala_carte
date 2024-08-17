package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildModule
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule

object Example_3_3:
    object BusyBuildModule extends BuildModule[Applicative, Unit, String, Int]:
        override val s: StoreModule[Unit, String, Int] = BusyStoreModule(
          Map("A1" -> 10, "A2" -> 20)
        )

        val busy: Build = (tasks, key, store) =>
            given Monad[StateMonad[s.Store]] = monadicState[s.Store]
            def fetch(k: String): State[s.Store, Int] =
                tasks(k) match
                    case None => State.gets(st => s.getValue(k, st))
                    case Some(task) =>
                        task(fetch).flatMap(v =>
                            State
                                .modify(st => s.putValue(k, v, st))
                                .flatMap(_ => v.ret)
                        )
            fetch(key).execState(store)

    class BusyStoreModule(val constants: Map[String, Int])
        extends StoreModule[Unit, String, Int]:
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
