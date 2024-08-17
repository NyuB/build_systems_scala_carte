package nyub.build_systems_a_la_carte

class TaskSuite extends munit.FunSuite:
    test("Example 3.2 - A1"):
        assertEquals(fetch("A1"), Some(1))

    test("Example 3.2 - A2"):
        assertEquals(fetch("A2"), Some(2))

    test("Example 3.2 - B1"):
        assertEquals(fetch("B1"), Some(1 + 2))

    test("Example 3.2 - B2"):
        assertEquals(fetch("B2"), Some(2 * (1 + 2)))

    def fetch(key: String): Option[Int] = key match
        // A1 and A2 are 'excel inputs'
        case "A1" => Some(1)
        case "A2" => Some(2)
        // If a cell is not an input, use tasks definitions to find the relevant task and run it to compute the cell
        case notAnInput =>
            Example_3_2.sprsh1(notAnInput).flatMap(task => task(fetch))

    given Applicative[Option] with
        override def pure[A](a: A): Option[A] = Some(a)
        extension [A, B](ff: Option[A => B])
            override def ap(fa: Option[A]): Option[B] = ff.flatMap(fa.map(_))

end TaskSuite
