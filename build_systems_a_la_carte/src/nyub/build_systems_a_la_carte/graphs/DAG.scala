package nyub.build_systems_a_la_carte.graphs

case class DAG[K] private (val graph: Map[K, Set[K]]):
    def contains(node: K): Boolean = graph.contains(node)
    def topologicalOrder(using Ordering[K]): Iterable[K] = topologicalOrder(List.empty)

    private def without(toRemove: Set[K]): DAG[K] =
        val newMap = graph.view.mapValues(_.diff(toRemove)).filterNot((k, _) => toRemove(k))
        new DAG(newMap.toMap)

    private def leaves: Set[K] = graph.filter(_._2.isEmpty).keySet

    private def topologicalOrder(using Ordering[K])(acc: List[K]): Iterable[K] = if graph.isEmpty then acc.reverse
    else
        val nextInOrder = leaves
        val sorted = nextInOrder.toSeq.sorted
        without(nextInOrder).topologicalOrder(sorted.foldLeft(acc)((a, i) => i :: a))

object DAG:
    def apply[K](edges: Map[K, Set[K]]): DAG[K] =
        if !DAG.acyclic(edges) then throw IllegalArgumentException("edges do not describe an acyclic graph")
        val graph: Map[K, Set[K]] = edges.values
            .flatMap(identity)
            .foldLeft(edges): (map, key) =>
                map.updatedWith(key):
                    case None          => Some(Set.empty)
                    case Some(already) => Some(already)
        new DAG(graph)

    def apply[K](root: K, edges: K => Set[K]): DAG[K] =
        val reachables = reachable(root, edges)
        val graph = reachables.toSeq.map(k => k -> edges(k)).toMap
        DAG(graph + (root -> reachables))

    private def acyclic[K](edges: Map[K, Set[K]]): Boolean =
        edges.keySet.forall(k => !reachable(k, edges.getOrElse(_, Set.empty)).contains(k))

    private def reachable[K](k: K, edges: K => Set[K]): Set[K] =
        reachable(k, edges, Set.empty)

    private def reachable[K](k: K, edges: K => Set[K], already: Set[K]): Set[K] =
        val descendants = edges(k)
        descendants
            .filterNot(already)
            .foldLeft(already): (r, d) =>
                reachable(d, edges, r + d)
