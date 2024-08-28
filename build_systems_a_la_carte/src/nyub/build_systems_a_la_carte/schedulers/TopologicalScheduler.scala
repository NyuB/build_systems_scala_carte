package nyub.build_systems_a_la_carte.schedulers

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.{BuildSystem, Rebuilder, Scheduler, StoreModule, Tasks}
import nyub.build_systems_a_la_carte.monads.Applicative
import nyub.build_systems_a_la_carte.graphs.DAG
import nyub.build_systems_a_la_carte.StaticDependencies

/** A scheduler using a topological ordering: leaves tasks are scheduled first, then their dependants tasks, etc
  *
  * Since this requires static knowledge of a given task dependencies, this scheduler accept only [[Applicative]] tasks
  *
  * @param ordering
  *   the ordering used to sort keys that are at the same topological level
  */
class TopologicalScheduler[I, K, V](using ordering: Ordering[K]) extends Scheduler[Applicative, I, I, K, V]:
    override def buildSystem(rebuilder: Rebuilder[Applicative, I, K, V]): BuildSystem[Applicative, I, K, V] = new:
        override def build(using
            storeModule: StoreModule[I, K, V]
        )(tasks: Tasks[Applicative, K, V], key: K, store: storeModule.Store): storeModule.Store =
            val dag = DAG(
              key,
              k => tasks(k).map(t => StaticDependencies.directDependencies(t)).getOrElse(Set.empty)
            )
            val order = dag.topologicalOrder
            FixOrderScheduler(order).buildSystem(rebuilder).build(tasks, key, store)
