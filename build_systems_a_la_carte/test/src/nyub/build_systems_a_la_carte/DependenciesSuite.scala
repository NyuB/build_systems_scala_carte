package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.StaticDependencies.{directDependencies => staticDirectDependencies}
import nyub.build_systems_a_la_carte.DynamicDependencies.{directDependencies => dynamicDirectDependencies}
import nyub.build_systems_a_la_carte.StaticDependencies.dependencies

class DependenciesSuite extends munit.FunSuite:
    test("B1 static direct dependencies in Example 3.2"):
        assertEquals(staticDirectDependencies(Example_3_2.taskB1), Set("A1", "A2"))

    test("B2 static direct dependencies in Example 3.2"):
        assertEquals(staticDirectDependencies(Example_3_2.taskB2), Set("B1"))

    test("B2 static dependencies in Example 3.2"):
        assertEquals(
          dependencies("B2", Example_3_2.sprsh1),
          Set("A1", "A2", "B1")
        )

    test("B1 dynamic dependencies when C1 == 1 in Example 3.5"):
        val Some(result -> dependencies) = dynamicDirectDependencies(fetchWithC1(1))(Example_3_5.taskB1): @unchecked
        assertEquals(result, A1)
        assertEquals(dependencies, Set("C1" -> 1, "B2" -> 42))

    test("B1 dynamic dependencies when C1 == 0 in Example 3.5"):
        val Some(result -> dependencies) = dynamicDirectDependencies(fetchWithC1(0))(Example_3_5.taskB1): @unchecked
        assertEquals(result, A2)
        assertEquals(dependencies, Set("C1" -> 0, "A2" -> 24))

    private def fetchWithC1(c1: Int)(k: String): Option[Int] = k match
        case "C1" => Some(c1)
        case "A1" => Some(A1)
        case "A2" => Some(A2)
        case _    => Example_3_5.sprsh2(k).flatMap(task => task(fetchWithC1(c1)))

    private def A1 = 42
    private def A2 = 24

end DependenciesSuite
