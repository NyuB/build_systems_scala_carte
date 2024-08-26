package nyub.build_systems_a_la_carte

import schedulers.TopologicalScheduler
import rebuilders.{ModificationTimes, ModTimeRebuilder}
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.monads.Applicative

/** [Make](https://www.gnu.org/software/make/) build system implementation by composition of a topological
  * [[BuildSystemsALaCarte.Scheduler]] and a modtimes-based [[BuildSystemsALaCarte.Rebuilder]]
  */
object Make:
    /** @param K
      *   tasks' keys type, must be orderable to build a topological order
      * @param V
      *   tasks' return type
      * @return
      *   a [[BuildSystem]] for applicative task using a topological scheduler (tasks without dependencies are built
      *   first, then their dependents, etc) and rebuilding tasks based on key modification times
      */
    def apply[K, V](using Ordering[K]): BuildSystem[Applicative, ModificationTimes[K], K, V] =
        TopologicalScheduler[ModificationTimes[K], K, V].buildSystem(ModTimeRebuilder[K, V])
