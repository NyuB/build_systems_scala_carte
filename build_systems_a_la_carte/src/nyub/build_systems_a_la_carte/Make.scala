package nyub.build_systems_a_la_carte

import schedulers.TopologicalScheduler
import rebuilders.{ModificationTimes, ModTimeRebuilder}
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.monads.Applicative

object Make:
    def apply[K, V](using Ordering[K]): BuildSystem[Applicative, ModificationTimes[K], K, V] =
        TopologicalScheduler[ModificationTimes[K], K, V].buildSystem(ModTimeRebuilder[K, V])
