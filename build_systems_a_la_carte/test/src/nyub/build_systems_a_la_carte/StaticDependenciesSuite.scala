package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.StaticDependencies.directDependencies
import nyub.build_systems_a_la_carte.StaticDependencies.dependencies

class StaticDependenciesSuite extends munit.FunSuite:
    test("B1 direct dependencies in Example 3.2"):
        assertEquals(directDependencies(Example_3_2.taskB1), Set("A1", "A2"))

    test("B2 direct dependencies in Example 3.2"):
        assertEquals(directDependencies(Example_3_2.taskB2), Set("B1"))

    test("B2 dependencies in Example 3.2"):
        assertEquals(
          dependencies("B2", Example_3_2.sprsh1),
          Set("A1", "A2", "B1")
        )

end StaticDependenciesSuite
