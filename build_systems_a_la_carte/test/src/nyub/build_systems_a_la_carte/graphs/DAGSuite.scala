package nyub.build_systems_a_la_carte.graphs

import org.scalacheck.Prop._

class DAGSuite extends munit.ScalaCheckSuite:
    test("Empty graph"):
        val dag = DAG(Map.empty)
        assertEquals(dag.topologicalOrder, Iterable.empty)

    property("Empty graph does not contain any node"):
        val dag = DAG[String](Map.empty)
        forAll: (s: String) =>
            assertEquals(dag.contains(s), false)

    test("Singleton graph"):
        val dag = DAG(Map("A" -> Set.empty))
        assertEquals(dag.topologicalOrder, Iterable("A"))
        assertEquals(dag.contains("A"), true)

    test("Direct cycle throws"):
        intercept[IllegalArgumentException]:
            DAG(Map("A" -> Set("A")))

    test("Cyclic pair throws"):
        intercept[IllegalArgumentException]:
            DAG(Map("A" -> Set("B"), "B" -> Set("A")))

    test("Indirect cycle throws"):
        intercept[IllegalArgumentException]:
            DAG(Map("A" -> Set("B"), "B" -> Set("C"), "C" -> Set("A")))

    test("Two to one topological order"):
        val dag = DAG(Map("A" -> Set("B", "C")))
        assertEquals(dag.topologicalOrder, Iterable("B", "C", "A"))

    test("From function constructor"):
        val dag = DAG("Root", _ => Set.empty)
        assertEquals(dag.contains("Root"), true)

end DAGSuite
